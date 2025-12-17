#include "parser.h"

#include "lexer.h"

#include <array>
#include <cstdint>
#include <expected>
#include <memory>
#include <print>
#include <string>
#include <string_view>
#include <utility>

namespace ast {

// Pre-Conditions: `kind` must be a valid binary operator
Expr::FunCall::FunCall(std::string_view fun,
                       std::vector<std::unique_ptr<Expr>> &args)
    : fun(fun), args(std::move(args)) {}

Expr::BinOp::BinOp(Expr::BinOp::Op op, std::unique_ptr<Expr> lhs,
                   std::unique_ptr<Expr> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

Expr::Expr(double num_lit) : kind(ExprTag::EXPR_NUM_LIT), num_lit(num_lit) {}
Expr::Expr(std::string_view var) : kind(ExprTag::EXPR_VAR), var(var) {}
Expr::Expr(FunCall &&fun_call)
    : kind(ExprTag::EXPR_FUN_CALL), fun_call(std::move(fun_call)) {}
Expr::Expr(BinOp &&bin_op)
    : kind(ExprTag::EXPR_BINARY_OP), bin_op(std::move(bin_op)) {}

static auto bop_to_string(Expr::BinOp::Op op) -> std::string {
    using Op = Expr::BinOp::Op;
    switch (op) {
    case Op::BINOP_LT:
        return "<";
    case Op::BINOP_LEQ:
        return "<=";
    case Op::BINOP_GT:
        return ">";
    case Op::BINOP_GEQ:
        return ">=";
    case Op::BINOP_EQ:
        return "==";
    case Op::BINOP_NEQ:
        return "!=";
    case Op::BINOP_ADD:
        return "+";
    case Op::BINOP_SUB:
        return "-";
    case Op::BINOP_MUL:
        return "*";
    case Op::BINOP_DIV:
        return "/";
    }
}

auto Expr::to_string() const -> std::string {
    switch (this->kind) {
    case ExprTag::EXPR_NUM_LIT:
        return std::to_string(this->num_lit);
    case ExprTag::EXPR_VAR:
        return std::string(this->var);
    case ExprTag::EXPR_FUN_CALL: {
        auto fun_call = &this->fun_call;
        auto res = std::string(fun_call->fun);
        res.push_back('(');
        auto args = std::span(fun_call->args.data(), fun_call->args.size());
        if (args.size() == 0) {
        } else if (args.size() == 1)
            res.append(args[0]->to_string());
        else {
            res.append(args[0]->to_string());
            for (auto &arg : args.subspan(1, args.size() - 1)) {
                res.append(", ");
                res.append(arg->to_string());
            }
        }
        res.push_back(')');
        return res;
    }
    case ExprTag::EXPR_BINARY_OP: {
        auto bin_op = &this->bin_op;
        std::string res = "(";
        res.append(bin_op->lhs->to_string());
        res.push_back(' ');
        res.append(bop_to_string(bin_op->op));
        res.push_back(' ');
        res.append(bin_op->rhs->to_string());
        res.push_back(')');
        return res;
    }
    }
}

Expr::~Expr() {
    switch (kind) {
    case ExprTag::EXPR_NUM_LIT:
        return;
    case ExprTag::EXPR_VAR:
        return;
    case ExprTag::EXPR_FUN_CALL:
        this->fun_call.~FunCall();
        return;
    case ExprTag::EXPR_BINARY_OP:
        this->bin_op.~BinOp();
        return;
    }
}
}; // namespace ast

using namespace ast;

Parser::Parser(const std::string_view source)
    : source(source), lexer(Lexer(source)) {}

auto Parser::parse() -> ParseResult<Expr> { return this->parse_expr(); }

auto Parser::parse_expr() -> ParseResult<Expr> {
    // auto token = this->lexer.next_token();
    // while (token.data != TokenKind::TOK_EOF) {
    //     std::println("{}", tokenkind_to_string(token.data));
    //     token = this->lexer.next_token();
    // }
    auto lhs = this->parse_basic_expr();
    if (!lhs.has_value())
        return std::unexpected(lhs.error());
    return this->parse_rhs(0, std::move(lhs.value()));
}

auto Parser::parse_basic_expr() -> ParseResult<Expr> {
    switch (this->lexer.peek_token()) {
    case TokenKind::TOK_NUMBER:
        return this->parse_num_lit();
    case TokenKind::TOK_LPAREN:
        return this->parse_grouping();
    case TokenKind::TOK_IDENT:
        return this->parse_identifier();
    default:
        return this->next_error<Expr>("expression");
    }
}

constexpr std::array<TokenKind, 10> BINARY_OPS = {
    TokenKind::TOK_LT,  TokenKind::TOK_LEQ, TokenKind::TOK_GT,
    TokenKind::TOK_GEQ, TokenKind::TOK_EQ,  TokenKind::TOK_NEQ,
    TokenKind::TOK_ADD, TokenKind::TOK_SUB, TokenKind::TOK_MUL,
    TokenKind::TOK_DIV,
};

using BinOp = Expr::BinOp::Op;

static auto tk_to_bop(TokenKind kind) -> BinOp {
    switch (kind) {
    case TokenKind::TOK_LT:
        return BinOp::BINOP_LT;
    case TokenKind::TOK_LEQ:
        return BinOp::BINOP_LEQ;
    case TokenKind::TOK_GT:
        return BinOp::BINOP_GT;
    case TokenKind::TOK_GEQ:
        return BinOp::BINOP_GEQ;
    case TokenKind::TOK_EQ:
        return BinOp::BINOP_EQ;
    case TokenKind::TOK_NEQ:
        return BinOp::BINOP_NEQ;
    case TokenKind::TOK_ADD:
        return BinOp::BINOP_ADD;
    case TokenKind::TOK_SUB:
        return BinOp::BINOP_SUB;
    case TokenKind::TOK_MUL:
        return BinOp::BINOP_MUL;
    case TokenKind::TOK_DIV:
        return BinOp::BINOP_DIV;
    default:
        std::unreachable();
    }
}

static auto get_op_precedence(BinOp op) -> std::uint8_t {
    switch (op) {
    case BinOp::BINOP_LT:
    case BinOp::BINOP_LEQ:
    case BinOp::BINOP_GT:
    case BinOp::BINOP_GEQ:
    case BinOp::BINOP_EQ:
    case BinOp::BINOP_NEQ:
        return 0;
    case BinOp::BINOP_ADD:
    case BinOp::BINOP_SUB:
        return 128;
    case BinOp::BINOP_MUL:
    case BinOp::BINOP_DIV:
        return 255;
    }
}

auto Parser::parse_rhs(std::uint8_t precedence, std::unique_ptr<ast::Expr> lhs)
    -> ParseResult<Expr> {
    while (this->at_any(BINARY_OPS)) {
        auto op = tk_to_bop(this->lexer.peek_token());
        auto op_prec = get_op_precedence(op);

        if (op_prec < precedence)
            return lhs;

        this->lexer.next_token(); // Skip `op`

        auto rhs_res = this->parse_basic_expr();
        if (!rhs_res.has_value())
            return std::unexpected(rhs_res.error());
        auto rhs = std::move(rhs_res.value());

        if (this->at_any(BINARY_OPS)) {
            std::println("bingus");
            auto next_op = tk_to_bop(this->lexer.peek_token());
            auto next_prec = get_op_precedence(next_op);

            if (op_prec < next_prec) {
                auto next_rhs_res =
                    this->parse_rhs(op_prec + 1, std::move(rhs));
                if (!next_rhs_res.has_value())
                    return std::unexpected(next_rhs_res.error());
                rhs = std::move(next_rhs_res.value());
            }
        }

        lhs = std::make_unique<Expr>(
            Expr::BinOp(op, std::move(lhs), std::move(rhs)));
    }

    return lhs;
}

auto Parser::parse_num_lit() -> ParseResult<Expr> {
    auto num_tok = this->lexer.next_token();
    auto num_str = num_tok.span.substr(this->source);

    double num;
    std::from_chars(num_str.begin(), num_str.end(), num);

    return ParseResult<Expr>(std::make_unique<Expr>(num));
}

auto Parser::parse_grouping() -> ParseResult<Expr> {
    this->advance(); // Skip '('

    auto inner = this->parse_expr();
    if (!inner.has_value())
        return std::unexpected(inner.error());

    auto err = this->expect(TokenKind::TOK_RPAREN);
    if (err.has_value())
        return std::unexpected(err.value());

    return inner;
}

auto Parser::parse_identifier() -> ParseResult<Expr> {
    auto token = this->lexer.next_token();
    auto name = token.span.substr(this->source);

    if (this->at(TokenKind::TOK_LPAREN)) {
        this->advance(); // Skip '('

        std::vector<std::unique_ptr<Expr>> args;
        while (!this->at(TokenKind::TOK_RPAREN)) {
            auto arg = this->parse_expr();
            if (arg.has_value())
                args.push_back(std::unique_ptr(std::move(arg.value())));
            else
                return std::unexpected(arg.error());

            if (this->lexer.peek_token() == TokenKind::TOK_COMMA)
                this->advance(); // Skip ','
            else
                break;
        }

        auto err = this->expect(TokenKind::TOK_RPAREN);
        if (err.has_value())
            return std::unexpected(err.value());

        return std::make_unique<Expr>(Expr::FunCall(name, args));
    } else
        return std::make_unique<Expr>(name);
}

inline auto Parser::at(TokenKind expected) -> bool {
    return this->lexer.peek_token() == expected;
}

template <std::size_t N>
inline auto Parser::at_any(std::array<TokenKind, N> expected) -> bool {
    return std::ranges::contains(expected, this->lexer.peek_token());
}

inline auto Parser::advance() -> void { this->lexer.next_token(); }

auto Parser::expect(TokenKind expected) -> std::optional<ParseError> {
    auto token = this->lexer.next_token();
    if (token.data == expected)
        return std::nullopt;
    else
        return std::make_pair(
            token.span, SyntaxError{tokenkind_to_string(expected), token.data});
}

template <class T>
inline auto Parser::next_error(std::string expected) -> ParseResult<T> {
    auto token = this->lexer.next_token();
    return std::unexpected(std::make_pair(
        token.span, SyntaxError{.expected = expected, .got = token.data}));
}
