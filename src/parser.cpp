#include "parser.h"

#include <algorithm>
#include <array>
#include <charconv>
#include <cstdint>
#include <expected>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <swl/variant.hpp>
#include <utility>
#include <vector>

#include "lexer.h"
#include "utils.h"

using namespace ast;

Parser::Parser(const std::string file_path, const std::string_view source)
    : file_path(file_path), source(source), lexer(Lexer(source)) {}

[[nodiscard]] auto Parser::parse_file() -> ParseResult<File> {
    auto items = std::vector<Item>();
    while (!this->at(TokenKind::TOK_EOF)) {
        switch (this->lexer.peek_token()) {
        case TokenKind::TOK_DEF: {
            auto def = this->parse_def();
            if (!def.has_value()) return std::unexpected(def.error());
            items.push_back(std::move(def.value()));
            break;
        }
        case TokenKind::TOK_EXTERN: {
            auto external = this->parse_extern();
            if (!external.has_value()) return std::unexpected(external.error());
            items.push_back(std::move(external.value()));
            break;
        }
        case TokenKind::TOK_SEMI: this->lexer.next_token(); break;
        default: {
            auto tle = this->parse_top_level_expr();
            if (!tle.has_value()) return std::unexpected(tle.error());
            items.push_back(std::move(tle.value()));
            break;
        }
        }
    }

    return File{ .file_path = this->file_path, .items = std::move(items) };
}

auto Parser::parse_def() -> ParseResult<Item> {
    this->advance(); // Skip 'def'
    auto proto_res = this->parse_proto();
    if (!proto_res.has_value()) return std::unexpected(proto_res.error());
    auto proto = proto_res.value();

    auto expr_res = this->parse_expr();
    if (!expr_res.has_value()) return std::unexpected(expr_res.error());

    return FunDef{ .proto = proto, .body = std::move(expr_res.value()) };
}

auto Parser::parse_extern() -> ParseResult<Item> {
    this->advance(); // Skip 'extern'
    auto proto_res = this->parse_proto();
    if (!proto_res.has_value()) return std::unexpected(proto_res.error());
    return Extern{ .proto = proto_res.value() };
}

auto Parser::parse_top_level_expr() -> ParseResult<Item> {
    return this->parse_expr().transform([](auto expr) {
        return TopLevelExpr{ .anon =
                                 FunDef{ .proto = Proto{ .name = "__top_level_expr", .args = {} },
                                         .body = std::move(expr) } };
    });
}

auto Parser::parse_proto() -> ParseResult<Proto> {
    auto token = this->lexer.next_token();
    if (token.data != TokenKind::TOK_IDENT)
        return this->next_error<Proto>(kind_to_string(token.data));
    auto name = token.span.source(this->source);

    auto lparen_err = this->expect(TokenKind::TOK_LPAREN);
    if (lparen_err.has_value()) return std::unexpected(lparen_err.value());
    auto args = std::vector<std::string_view>();
    while (this->at(TokenKind::TOK_IDENT)) {
        auto arg = this->lexer.next_token().span.source(this->source);
        args.push_back(arg);
        // no comma ??, questionable syntax decision
    }
    auto rparen_err = this->expect(TokenKind::TOK_RPAREN);
    if (rparen_err.has_value()) return std::unexpected(rparen_err.value());

    return Proto{ .name = name, .args = std::move(args) };
}

auto Parser::parse_expr() -> ParseResultBoxed<Expr> {
    // // Leaving this in in case we need to debug the lexer
    // auto token = this->lexer.next_token();
    // while (token.data != TokenKind::TOK_EOF) {
    //     std::println("{}", tokenkind_to_string(token.data));
    //     token = this->lexer.next_token();
    // }

    auto lhs = this->parse_basic_expr();
    if (!lhs.has_value()) return std::unexpected(lhs.error());
    return this->parse_rhs(0, std::move(lhs.value()));
}

auto Parser::parse_basic_expr() -> ParseResultBoxed<Expr> {
    switch (this->lexer.peek_token()) {
    case TokenKind::TOK_NUMBER: return this->parse_num_lit();
    case TokenKind::TOK_LPAREN: return this->parse_grouping();
    case TokenKind::TOK_IF: return this->parse_if_expr();
    case TokenKind::TOK_FOR: return this->parse_for_expr();
    case TokenKind::TOK_VAR: return this->parse_var_expr();
    case TokenKind::TOK_IDENT: return this->parse_ident_or_call();
    default: return this->next_error<Box<Expr>>("expression");
    }
}

constexpr std::array<TokenKind, 12> BINARY_OPS = {
    TokenKind::TOK_COLON, TokenKind::TOK_ASSIGN, TokenKind::TOK_LT,  TokenKind::TOK_LEQ,
    TokenKind::TOK_GT,    TokenKind::TOK_GEQ,    TokenKind::TOK_EQ,  TokenKind::TOK_NEQ,
    TokenKind::TOK_ADD,   TokenKind::TOK_SUB,    TokenKind::TOK_MUL, TokenKind::TOK_DIV,
};

// Pre-Condition: `kind` must be a recognised binary operator
static auto tk_to_bop(TokenKind kind) -> BinOp::Op {
    using Op = BinOp::Op;
    switch (kind) {
    case TokenKind::TOK_COLON: return Op::BINOP_SEQ;
    case TokenKind::TOK_ASSIGN: return Op::BINOP_ASS;
    case TokenKind::TOK_LT: return Op::BINOP_LT;
    case TokenKind::TOK_LEQ: return Op::BINOP_LEQ;
    case TokenKind::TOK_GT: return Op::BINOP_GT;
    case TokenKind::TOK_GEQ: return Op::BINOP_GEQ;
    case TokenKind::TOK_EQ: return Op::BINOP_EQ;
    case TokenKind::TOK_NEQ: return Op::BINOP_NEQ;
    case TokenKind::TOK_ADD: return Op::BINOP_ADD;
    case TokenKind::TOK_SUB: return Op::BINOP_SUB;
    case TokenKind::TOK_MUL: return Op::BINOP_MUL;
    case TokenKind::TOK_DIV: return Op::BINOP_DIV;
    default: std::unreachable();
    }
}

static auto get_op_precedence(BinOp::Op op) -> std::uint8_t {
    using Op = BinOp::Op;
    switch (op) {
    case Op::BINOP_SEQ: return 0;
    case Op::BINOP_ASS: return 16;
    case Op::BINOP_LT:
    case Op::BINOP_LEQ:
    case Op::BINOP_GT:
    case Op::BINOP_GEQ:
    case Op::BINOP_EQ:
    case Op::BINOP_NEQ: return 32;
    case Op::BINOP_ADD:
    case Op::BINOP_SUB: return 64;
    case Op::BINOP_MUL:
    case Op::BINOP_DIV: return 128;
    }
}

auto Parser::parse_rhs(std::uint8_t precedence, Box<Expr> lhs) -> ParseResultBoxed<Expr> {
    while (this->at_any(BINARY_OPS)) {
        auto op = tk_to_bop(this->lexer.peek_token());
        auto op_prec = get_op_precedence(op);

        if (op_prec < precedence) return lhs;

        this->advance(); // Skip `op`

        auto rhs_res = this->parse_basic_expr();
        if (!rhs_res.has_value()) return std::unexpected(rhs_res.error());
        auto rhs = std::move(rhs_res.value());

        if (this->at_any(BINARY_OPS)) {
            auto next_op = tk_to_bop(this->lexer.peek_token());
            auto next_prec = get_op_precedence(next_op);

            if (op_prec < next_prec) {
                auto next_rhs_res = this->parse_rhs(op_prec + 1, std::move(rhs));
                if (!next_rhs_res.has_value()) return std::unexpected(next_rhs_res.error());
                rhs = std::move(next_rhs_res.value());
            }
        }
        lhs =
            std::make_unique<Expr>(BinOp{ .op = op, .lhs = std::move(lhs), .rhs = std::move(rhs) });
    }

    return lhs;
}

auto Parser::parse_num_lit() -> ParseResultBoxed<Expr> {
    auto num_tok = this->lexer.next_token();
    auto num_str = num_tok.span.source(this->source);

    double num;
    std::from_chars(num_str.begin(), num_str.end(), num);

    return std::make_unique<Expr>(NumLit{ .value = num });
}

auto Parser::parse_grouping() -> ParseResultBoxed<Expr> {
    this->advance(); // Skip '('

    auto inner = this->parse_expr();
    if (!inner.has_value()) return std::unexpected(inner.error());

    auto rparen_err = this->expect(TokenKind::TOK_RPAREN);
    if (rparen_err.has_value()) return std::unexpected(rparen_err.value());

    return inner;
}

auto Parser::parse_ident_or_call() -> ParseResultBoxed<Expr> {
    auto token = this->lexer.next_token();
    auto name = token.span.source(this->source);

    if (this->at(TokenKind::TOK_LPAREN)) {
        this->advance(); // Skip '('

        auto args = std::vector<Box<Expr>>();
        while (!this->at(TokenKind::TOK_RPAREN)) {
            auto arg = this->parse_expr();
            if (arg.has_value()) args.push_back(Box(std::move(arg.value())));
            else return std::unexpected(arg.error());

            if (this->lexer.peek_token() == TokenKind::TOK_COMMA) this->advance(); // Skip ','
            else break;
        }

        auto rparen_err = this->expect(TokenKind::TOK_RPAREN);
        if (rparen_err.has_value()) return std::unexpected(rparen_err.value());

        return std::make_unique<Expr>(FunCall{ .fun = name, .args = std::move(args) });
    } else return std::make_unique<Expr>(Var{ .name = name });
}

auto Parser::parse_if_expr() -> ParseResultBoxed<Expr> {
    this->advance();
    auto cond_res = this->parse_expr();
    if (!cond_res.has_value()) return std::unexpected(cond_res.error());

    auto then_err = this->expect(TokenKind::TOK_THEN);
    if (then_err.has_value()) return std::unexpected(then_err.value());

    auto then_res = this->parse_expr();
    if (!then_res.has_value()) return std::unexpected(cond_res.error());

    auto else_err = this->expect(TokenKind::TOK_ELSE);
    if (else_err.has_value()) return std::unexpected(else_err.value());

    auto else_res = this->parse_expr();
    if (!else_res.has_value()) return std::unexpected(else_res.error());

    return std::make_unique<Expr>(IfExpr{ .cond = std::move(cond_res.value()),
                                          .then = std::move(then_res.value()),
                                          .else_ = std::move(else_res.value()) });
}

auto Parser::parse_for_expr() -> ParseResultBoxed<Expr> {
    this->advance();

    auto counter_tok = this->lexer.next_token();
    if (counter_tok.data != TokenKind::TOK_IDENT)
        return this->next_error<Box<Expr>>(kind_to_string(counter_tok.data));
    auto counter = counter_tok.span.source(this->source);

    auto assign_err = this->expect(TokenKind::TOK_ASSIGN);
    if (assign_err.has_value()) return std::unexpected(assign_err.value());

    auto start_res = this->parse_expr();
    if (!start_res.has_value()) return std::unexpected(start_res.error());

    auto comma_err = this->expect(TokenKind::TOK_COMMA);
    if (comma_err.has_value()) return std::unexpected(comma_err.value());

    auto end_res = this->parse_expr();
    if (!end_res.has_value()) return std::unexpected(end_res.error());

    std::optional<Box<Expr>> step = std::nullopt;
    if (this->at(TokenKind::TOK_COMMA)) {
        this->advance();
        auto step_res = this->parse_expr();
        if (!step_res.has_value()) return std::unexpected(step_res.error());
        step = std::move(step_res.value());
    }

    auto in_err = this->expect(TokenKind::TOK_IN);
    if (in_err.has_value()) return std::unexpected(in_err.value());

    auto body_res = this->parse_expr();
    if (!body_res.has_value()) return std::unexpected(body_res.error());

    return std::make_unique<Expr>(ForExpr{ counter, std::move(start_res.value()),
                                           std::move(end_res.value()), std::move(step),
                                           std::move(body_res.value()) });
}

auto Parser::parse_var_bind() -> ParseResult<VarExprBinding> {
    if (!this->at(TokenKind::TOK_IDENT))
        return this->next_error<VarExprBinding>("Expected variable binding");
    auto name_tok = this->lexer.next_token();
    auto name = name_tok.span.source(this->source);

    std::optional<Box<Expr>> value = std::nullopt;
    if (this->at(TokenKind::TOK_ASSIGN)) {
        this->advance();
        auto value_res = this->parse_expr();
        if (!value_res.has_value()) return std::unexpected(value_res.error());
        value = std::move(value_res.value());
    }

    return VarExprBinding{ .name = name, .value = std::move(value) };
}

auto Parser::parse_var_expr() -> ParseResultBoxed<Expr> {
    this->advance(/* Eat 'var' */);

    if (!this->at(TokenKind::TOK_IDENT))
        return this->next_error<Box<Expr>>("Variable expression requires at least one binding");

    auto first_bind_res = this->parse_var_bind();
    if (!first_bind_res.has_value()) return std::unexpected(first_bind_res.error());

    auto bindings = std::vector<VarExprBinding>();
    while (this->at(TokenKind::TOK_COMMA)) {
        this->advance(/* Eat comma */);
        auto binding_res = this->parse_var_bind();
        if (!binding_res.has_value()) return std::unexpected(binding_res.error());
        bindings.push_back(std::move(binding_res.value()));
    }

    auto in_err = this->expect(TokenKind::TOK_IN);
    if (in_err.has_value()) return std::unexpected(in_err.value());

    auto body_res = this->parse_expr();
    if (!body_res.has_value()) return std::unexpected(body_res.error());

    return std::make_unique<Expr>(VarExpr{ .first_bind = std::move(first_bind_res.value()),
                                           .bindings = std::move(bindings),
                                           .body = std::move(body_res.value()) });
}

inline auto Parser::at(TokenKind expected) -> bool { return this->lexer.peek_token() == expected; }

template <std::size_t N> inline auto Parser::at_any(std::array<TokenKind, N> expected) -> bool {
    return std::ranges::contains(expected, this->lexer.peek_token());
}

inline auto Parser::advance() -> void { this->lexer.next_token(); }

auto Parser::expect(TokenKind expected) -> std::optional<ParseError> {
    auto token = this->lexer.next_token();
    if (token.data == expected) return std::nullopt;
    else return std::make_pair(token.span, SyntaxError{ kind_to_string(expected), token.data });
}

template <class T> inline auto Parser::next_error(std::string message) -> ParseResult<T> {
    auto token = this->lexer.next_token();
    return std::unexpected(
        std::make_pair(token.span, SyntaxError{ .expected = message, .got = token.data }));
}

namespace ast {
static auto bop_to_string(BinOp::Op op) -> std::string {
    using Op = BinOp::Op;
    switch (op) {
    case Op::BINOP_SEQ: return ":";
    case Op::BINOP_ASS: return "=";
    case Op::BINOP_LT: return "<";
    case Op::BINOP_LEQ: return "<=";
    case Op::BINOP_GT: return ">";
    case Op::BINOP_GEQ: return ">=";
    case Op::BINOP_EQ: return "==";
    case Op::BINOP_NEQ: return "!=";
    case Op::BINOP_ADD: return "+";
    case Op::BINOP_SUB: return "-";
    case Op::BINOP_MUL: return "*";
    case Op::BINOP_DIV: return "/";
    }
}

auto ExprPrinter::operator()(const NumLit &kind) const -> std::string {
    return std::to_string(kind.value);
}

auto ExprPrinter::operator()(const Var &kind) const -> std::string {
    return std::string(kind.name);
}

auto ExprPrinter::operator()(const FunCall &kind) const -> std::string {
    auto fun_call = &kind;
    auto res = std::string(fun_call->fun);
    res.push_back('(');
    auto args = std::span(fun_call->args.data(), fun_call->args.size());
    if (args.size() == 0) {
    } else if (args.size() == 1) res.append(expr_to_string(*args[0]));
    else {
        res.append(expr_to_string(*args[0]));
        for (auto &arg : args.subspan(1, args.size() - 1)) {
            res.append(", ");
            res.append(expr_to_string(*arg));
        }
    }
    res.push_back(')');
    return res;
}

auto ExprPrinter::operator()(const BinOp &kind) const -> std::string {
    auto bin_op = &kind;
    std::string res = "(";
    res.append(expr_to_string(*bin_op->lhs));
    res.push_back(' ');
    res.append(bop_to_string(bin_op->op));
    res.push_back(' ');
    res.append(expr_to_string(*bin_op->rhs));
    res.push_back(')');
    return res;
}

auto ExprPrinter::operator()(const IfExpr &kind) const -> std::string {
    auto ife = &kind;
    std::string res = "(if ";
    res.append(expr_to_string(*ife->cond));
    res.append(" then ");
    res.append(expr_to_string(*ife->then));
    res.append(" else ");
    res.append(expr_to_string(*ife->else_));
    res.push_back(')');
    return res;
}

auto ExprPrinter::operator()(const ForExpr &kind) const -> std::string {
    auto fore = &kind;
    std::string res = "(for ";
    res.append(fore->counter);
    res.append(" = ");
    res.append(expr_to_string(*fore->start));
    res.append(", ");
    res.append(expr_to_string(*fore->end));
    if (fore->step.has_value()) {
        res.append(", ");
        res.append(expr_to_string(*fore->step.value()));
    }
    res.append(" in ");
    res.append(expr_to_string(*fore->body));
    res.push_back(')');
    return res;
}

static auto var_expr_bind_to_string(const VarExprBinding &bind) -> std::string {
    auto res = std::string(bind.name);
    if (bind.value.has_value()) {
        res.append(" = ");
        res.append(expr_to_string(*bind.value.value()));
    }
    return res;
}

auto ExprPrinter::operator()(const VarExpr &kind) const -> std::string {
    auto var = &kind;
    std::string res = "(var ";
    res.append(var_expr_bind_to_string(var->first_bind));
    for (auto &bind : var->bindings) {
        res.append(", ");
        res.append(var_expr_bind_to_string(bind));
    }
    res.append(" in ");
    res.append(expr_to_string(*var->body));
    res.push_back(')');
    return res;
}

auto expr_to_string(const Expr &expr) -> std::string { return swl::visit(ExprPrinter{}, expr); }

static auto proto_to_string(const Proto &proto) -> std::string {
    auto res = std::string(proto.name);
    res.push_back('(');
    auto args = std::span(proto.args);
    if (args.size() == 0) {
    } else if (args.size() == 1) res.append(args[0]);
    else {
        res.append(args[0]);
        for (auto arg : args.subspan(1, args.size() - 1)) {
            res.append(" ");
            res.append(arg);
        }
    }
    res.push_back(')');

    return res;
}

auto ItemPrinter::operator()(const FunDef &kind) const -> std::string {
    auto res = std::string("def ");
    res.append(proto_to_string(kind.proto));
    res.push_back(' ');
    res.append(expr_to_string(*kind.body));

    return res;
}
auto ItemPrinter::operator()(const Extern &kind) const -> std::string {
    return std::string("extern ") + proto_to_string(kind.proto);
}
auto ItemPrinter::operator()(const TopLevelExpr &kind) const -> std::string {
    return expr_to_string(*kind.anon.body);
}
}; // namespace ast
