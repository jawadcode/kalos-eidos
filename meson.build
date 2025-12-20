project(
  'kalos-eidos',
  ['c', 'cpp'],
  default_options: ['warning_level=3', 'cpp_std=c++23'],
)

llvm_dep = dependency('llvm', version: '>= 18.0', static: true)

executable(
  'kalos-eidos',
  sources: [
    'src/utils.cpp',
    'src/lexer.cpp',
    'src/parser.cpp',
    'src/compiler.cpp',
    'src/cli.cpp',
    'src/main.cpp',
  ],
  include_directories: include_directories('include'),
  dependencies: [llvm_dep],
)
