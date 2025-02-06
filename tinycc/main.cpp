#include <cstdlib>
#include <cstring>
#include <iostream>

#include "common/options.h"
#include "common/colors.h"
#include "frontend/parser.h"
#include "frontend/typechecker.h"
#include "optimizer/ast_to_il.h"
#include "backend/target_translator.h"

using namespace tiny;
using namespace colors;

struct Test {
    char const * file;
    int line;
    char const * input;
    int64_t result = 0;
    bool testResult = false;
    char const * shouldError = nullptr;

    Test(char const * file, int line, char const * input):
        file{file}, line{line}, input{input} {
    }

    Test(char const * file, int line, char const * input, int64_t result):
        file{file}, line{line}, input{input}, result{result}, testResult{true} {
    }

    Test(char const * file, int line, char const * input, int result, bool testResult, char const * shouldError):
        file{file}, line{line}, input{input}, result{result}, testResult{testResult}, shouldError{shouldError} {
    }
};

#define TEST(...) Test{__FILE__, __LINE__, __VA_ARGS__}
#define ERROR(input, kind) Test{__FILE__, __LINE__, input, 0, false, # kind} 

Test tests[] = {
//        TEST("// Forward declaration of struct\n"
//             "struct Rectangle;\n"
//             "\n"
//             "// Struct declaration\n"
//             "struct Point {\n"
//             "    int x;\n"
//             "    int y;\n"
//             "};\n"
//             "\n"
//             "// Complete struct declaration for Rectangle\n"
//             "struct Rectangle {\n"
//             "    Point topLeft;\n"
//             "    Point bottomRight;\n"
//             "};\n"
//             "\n"
//             "// Function declaration\n"
//             "\n"
//             "// Function definition\n"
//             "int calculateArea(Rectangle rect) {\n"
//             "    int width = rect.bottomRight.x - rect.topLeft.x;\n"
//             "    int height = rect.bottomRight.y - rect.topLeft.y;\n"
//             "    return width * height;\n"
//             "}\n"
//             "typedef int (*calculate_ptr)(Rectangle);"
//             "\n"
//             "// Function declaration\n"
//             "\n"
//             "// Function definition\n"
//             "int isPointInside(Rectangle rect, Point p) {\n"
//             "    return p.x >= rect.topLeft.x && p.x <= rect.bottomRight.x &&\n"
//             "           p.y >= rect.topLeft.y && p.y <= rect.bottomRight.y;\n"
//             "}\n"
//             "// Function definition\n"
//             "void generateSpiralPoints(Point* points, int n) {\n"
//             "    int x = 0; int y = 0;\n"
//             "    int dx = 1; int dy = 0;\n"
//             "    int step = 1;\n"
//             "    int index = 0;\n"
//             "\n"
//             "    for (int i = 0; i < n; i++) {\n"
//             "        points[index].x = x;\n"
//             "        points[index].y = y;\n"
//             "        index++;\n"
//             "\n"
//             "        if (index % step == 0) {\n"
//             "            int temp = dx;\n"
//             "            dx = -dy;\n"
//             "            dy = temp;\n"
//             "            if (dy == 0) step++;\n"
//             "        }\n"
//             "\n"
//             "        x = x + dx;\n"
//             "        y = y + dy;\n"
//             "    }\n"
//             "}\n"
//             "\n"
//             "// Main function\n"
//             "int main() {\n"
//             "    calculate_ptr ptr = &calculateArea;\n"
//             "    "
//             "    Rectangle rect;\n"
//             "    Point testPoint;\n"
//             "    Point spiralPoints[10];\n"
//             "\n"
//             "    int inside = isPointInside(rect, testPoint);\n"
//             "\n"
//             "    generateSpiralPoints(spiralPoints, 10);\n"
//             "\n"
//             "    for (int i = 0; i < 10; i++) {\n"
//             "        Point p = spiralPoints[i];\n"
//             "        // Logic to use point (p.x, p.y) could be added here\n"
//             "    }\n"
//             "\n"
//             "    return 0;\n"
//             "}", 0),
//        TEST("int foo (int a, int b) {return a + b; } int main() {return foo( 1, 2 );}"),
//        TEST("int main(int a, int b, int c) { b = ++a; c = a++;}", 0),
//        TEST("int main() { int b[10]; }", 0),
//        TEST("void increment(int * x) {"
//             "*x = *x + 1;}"
//             "int main() { int x = 0; while(x < 100) {increment(&x); printi(-x);} "
//             "return 0; }", 0),
//        TEST("int main() { char * str = \"test\"; char* str2 = \"second str\"; for (int i = 0; i < 4; i++) {printc(str[i]);}\n"
//             "for (int i = 0; i < 10; i++) {printc(str2[i]);} return 0;}", 0),
//        TEST("int main() {\n"
//             "  int x[10];"
//             "x[1] = 2; printi(x[1]);"
//             "return 0;"
//             "}", 0),
//    TEST("\n"
//         "struct Point {\n"
//         "  int a;\n"
//         "  int b;\n"
//         "};\n"
//         "\n"
//         "struct Rectangle {\n"
//         "  Point a;\n"
//         "  Point b;\n"
//         "};\n"
//         "\n"
//         "int main () {\n"
//         "  Rectangle rect;\n"
//         "  rect.a.a = 2;\n"
//         "  rect.b.a = 3;\n"
//         "  rect.b.a = 4;\n"
//         "  rect.b.b = 4;\n"
//         "  printi(rect.b.b);\n"
//         "  return 0;\n"
//         "}", 0),
    TEST(
         "\n"
         "// Binary Search function\n"
         "int binarySearch(int* arr, int size, int target) {\n"
         "    int low = 0;\n"
         "    int high = size - 1;\n"
         "    \n"
         "    while (low <= high) {\n"
         "    int mid = high - low; mid = mid / 2; mid = mid + low;\n"
         "        \n"
         "        // Check if target is present at mid\n"
         "        if (arr[mid] == target) {\n"
         "            return mid; // Target found, return its index\n"
         "        }\n"
         "        \n"
         "        if (arr[mid] < target) {\n"
         "            low = mid + 1;\n"
         "        }\n"
         "       \n"
         "        else {\n"
         "            high = mid - 1;\n"
         "        }\n"
         "    }\n"
         "    \n"
         "    return -1; \n"
         "}\n"
         "\n"
         "int main() {\n"
         "    int arr[10];"
         "int size = 10;\n"
         "    for (int i = 0; i < size; i++) {\n"
         "        arr[i] = 2 * i + 1;\n"
         "    }\n"
         "    int target = 4;\n"
         "\n"
         "    int result = binarySearch(arr, size, target);\n"
         "printi(result);"
         "    \n"
         "\n"
         "    return 0;\n"
         "}", 10),
//    TEST("int main() { if (0) return 10; else return 2; }", 2),
//    TEST("int main() { int i = 1; return i; }", 1),
//    TEST("int bar(int i) { return i; } int main() { return bar(5); }", 5),
//    TEST("int bar(int i) { if (i) return 10; else return 5; } int main() { return bar(5); }", 10),
//    TEST("void bar(int * i) { *i = 10; } int main() { int i = 1; bar(&i); return i; }", 10),

  #ifdef foo  
/*    TEST("void main(int a, int b) {}"),
    TEST("void main(int a, int b) { 1 * 2; }"),
    TEST("void main(int a, int b) { a * b; }"),
    TEST("void main(int a, int b) { a = 3; }"),
    TEST("void main(int a, int b) { a = b; }"),
*/
    // Tests that exercise the parser, lexer and a typechecker
    TEST("void main() {}"),
    ERROR("void main() { return 1; }", TypeError), 
    TEST("void main() { 1 * 2; }"),
    TEST("void main() { int a; }"),
    TEST("int main() { return 1; }"),
    TEST("void main() { return; }"),
    //ERROR("void main() { break; }", ParserError),
    TEST("int main() { int a; return a; }"),
    TEST("double main() { double a; return a; }"),
    TEST("int main() { if (1) return 1; else return 2; }"),
    ERROR("int main() { if (1) return 1; else return 2.0; }", TypeError),
    ERROR("void main() { a = 5; }", TypeError),
    TEST("void main() { int a; a = 7; }"),
    ERROR("void main() { int a; a = 7.3; }", TypeError),
    TEST("int* main() { int a; return & a; }"),
    ERROR("void* main() { int a; return & a; }", TypeError),
    TEST("int main() { int * a; return *a; }"),
    TEST("int main(int a) { return a; }"),
    TEST("int foo(int a, int b) { return a; } int main() { return foo(1, 2); }"),
    TEST("int* main() { return cast<int*>(0); }"),
    ERROR("int foo(int a, int b) { return a; } int main() { return foo(1, 'a'); }", TypeError),
    ERROR("int foo(int a, int b) { return a; } int main() { return foo(1); }", TypeError),
    ERROR("int foo(int a, int b) { return a; } int main() { return bar(1, 2); }", TypeError),
    TEST("int main(int x) { return main(x); }"),
    TEST("int a = 56; int main() { return a; }"),
    TEST("double b = 6.7; int main() { int a = 1; return a; }"),
    TEST("struct Foo { }; void main(Foo x) {}"),
    ERROR("struct Foo; void main(Foo x) {}", TypeError),
    TEST("struct Foo; struct Foo { int i; }; void main(Foo x) {}"),
    ERROR("struct Foo; struct Foo { int i; }; struct Foo { int i; }; void main(Foo x) {}", TypeError),
    TEST("int main(int argc) { return argc++; }"),
    TEST("double main(double argc) { return argc++; }"),
    TEST("char main(char argc) { return argc++; }"),
    TEST("int * main(int * argc) { return argc++; }"),
    ERROR("struct Foo {}; Foo main(Foo argc) { return argc++; }", TypeError),
    TEST("int main(int argc) { return ++argc; }"),
    TEST("double main(double argc) { return ++argc; }"),
    TEST("char main(char argc) { return ++argc; }"),
    TEST("int * main(int * argc) { return ++argc; }"),
    ERROR("struct Foo {}; Foo main(Foo argc) { return ++argc; }", TypeError),
    TEST("int main(int i) { return +i; }"),
    TEST("int main(int i) { return -i; }"),
    TEST("int main(int i) { return ~i; }"),
    TEST("int main(int i) { return !i; }"),
    TEST("int main(int i) { return ++i; }"),
    TEST("int main(int i) { return --i; }"),
    TEST("double main(double i) { return +i; }"),
    TEST("double main(double i) { return -i; }"),
    ERROR("double main(double i) { return ~i; }", TypeError),
    TEST("int main(double i) { return !i; }"),
    TEST("double main(double i) { return ++i; }"),
    TEST("double main(double i) { return --i; }"),
    TEST("char main(char i) { return +i; }"),
    TEST("char main(char i) { return -i; }"),
    TEST("char main(char i) { return ~i; }"),
    TEST("int main(char i) { return !i; }"),
    TEST("char main(char i) { return ++i; }"),
    TEST("char main(char i) { return --i; }"),
    ERROR("int * main(int * i) { return +i; }", TypeError),
    ERROR("int * main(int * i) { return -i; }", TypeError),
    ERROR("int * main(int * i) { return ~i; }", TypeError),
    TEST("int main(int * i) { return !i; }"),
    TEST("int * main(int * i) { return ++i; }"),
    TEST("int * main(int * i) { return --i; }"),
    ERROR("struct Foo {}; Foo main(Foo i) { return +i; }", TypeError),
    ERROR("struct Foo {}; Foo main(Foo i) { return -i; }", TypeError),
    ERROR("struct Foo {}; Foo main(Foo i) { return ~i; }", TypeError),
    ERROR("struct Foo {}; Foo main(Foo i) { return !i; }", TypeError),
    ERROR("struct Foo {}; Foo main(Foo i) { return ++i; }", TypeError),
    ERROR("struct Foo {}; Foo main(Foo i) { return --i; }", TypeError),
    TEST("int main(int a, int b) { return a + b; }"),
    TEST("int main(int a, int b) { return a - b; }"),
    TEST("int main(int a, int b) { return a * b; }"),
    TEST("int main(int a, int b) { return a / b; }"),
    TEST("int main(int a, int b) { return a % b; }"),
    TEST("int main(int a, int b) { return a << b; }"),
    TEST("int main(int a, int b) { return a >> b; }"),
    TEST("int main(int a, int b) { return a > b; }"),
    TEST("int main(int a, int b) { return a < b; }"),
    TEST("int main(int a, int b) { return a >= b; }"),
    TEST("int main(int a, int b) { return a <= b; }"),
    TEST("int main(int a, int b) { return a == b; }"),
    TEST("int main(int a, int b) { return a != b; }"),
    TEST("int main(int a, int b) { return a & b; }"),
    TEST("int main(int a, int b) { return a | b; }"),
    TEST("int main(int a, int b) { return a && b; }"),
    TEST("int main(int a, int b) { return a || b; }"),
    TEST("int main(double a, double b) { return a > b; }"),
    TEST("int main(double a, int b) { return a > b; }"),
    TEST("int main(int a, double b) { return a > b; }"),
    TEST("int main(int * a, int * b) { return a > b; }"),
    TEST("double main(int a, double b) { return a + b; }"),
    TEST("int * main(int a, int * b) { return b + a; }"),
    TEST("void main(int a) { a = cast<int>(4.0); }"),
    ERROR("void main() { 5 = 6; }", TypeError),
    TEST("void main(double * a) { *a = 6.0; }"),
    TEST("void main(int * a) { a = 678; }"),
    ERROR("void main(int a) { & a = 678; }", TypeError),
    ERROR("int main() {}", TypeError),
    ERROR("int main(int a) { if (a) { return 1; }}", TypeError),
    TEST("int main(int a) { if (a) { return 1; } else { return 2; }}"),
    ERROR("int main(int a) { while (a < 10) { return 1; }}", TypeError), 
    TEST("int main(int a) { do { return 1; } while (a); }"), 
    TEST("int main(int a) { return 1; if (a) { return 2; }}"),

    TEST("void main() { scan(); }"),
    TEST("void main() { print('a'); }"),
    // scan and print and intrinsic functions, i.e. they are handled specially by the compiler. This allows us to deal with them in different ways than normal functions, such as we can overload based on the print argument, which is something tinyC does not support for functions. However, our handling of intrinsics in the typechecker is to create a "fake" functions for them so this does not work atm. A good extra HW is to make this test work:
    ERROR("void main() { print(67); }", TypeError),

    // some full program tests
    TEST("int main(){ \
        int a = 4; \
        a = cast<int>(scan()); \
        print(cast<char>(a)); \
        return a; \
    }"),
    TEST("int fact(int n) { \
        if (n == 1) \
            return 1; \
        else \
            return fact(n - 1) * n; \
    } \
    int main() { \
        return fact(10); \
    }"),
#endif
};

bool compile(std::string const & contents, Test const * test) {
    try {
        // parse
        std::unique_ptr<AST> ast = (test == nullptr) ? Parser::parseFile(contents) : Parser::parse(contents);
        if (Options::verboseAST)
            std::cout << ColorPrinter::colorize(*ast) << std::endl;
        // typecheck
        Typechecker::checkProgram(ast);
        // translate to IR
        Program p = ASTToILTranslator::translateProgram(ast);
        assert(p.main != nullptr);
        // optimize
        // TODO

        // translate to target
        TargetTranslator translator(p);
        translator.translateToTarget("/Users/kirleo/Documents/gen/code-gen/tinycc/output.t86");
        return (test == nullptr) || ! (test->shouldError);
    } catch (SourceError const & e) {
        if ((test != nullptr) && test->shouldError == e.kind())
            return true;
        std::cerr << color::red << "ERROR: " << color::reset << e << std::endl;
    } catch (std::exception const &e) {
        std::cerr << color::red << "ERROR: " << color::reset << e.what() << std::endl;
    } catch (...) {
        std::cerr << color::red << "UNKNOWN ERROR. " << color::reset << std::endl;
    }
    return false;
}

int main(int argc, char * argv []) {
    initializeTerminal();
    std::cout << color::gray << "The one and only Tiny-C brought to you by NI-GEN" << color::reset << std::endl;
    // parse arguments
    char const * filename = nullptr;
    if (Options::parseArgs(argc, argv, filename) == EXIT_FAILURE)
        return EXIT_FAILURE;
    if (filename == nullptr) {
        size_t ntests = sizeof(tests) / sizeof(Test);
        size_t fails = 0;
        std::cout << "Running " << ntests << " tests..." << std::endl;
        for (auto const & t : tests) {
            if (! compile(t.input, & t)) {
                std::cout << color::red << t.file << ":" << t.line << ": Test failed." << color::reset << std::endl;
                std::cout << "    " << t.input << std::endl;
                ++fails;
                if (Options::exitAfterFailure)
                    break;
            }
        }
        if (fails > 0) {
            std::cout << color::red << "FAIL. Total " << fails << " tests out of " << ntests << " failed." << color::reset << std::endl;
            return EXIT_FAILURE;
        } else {
            std::cout << color::green << "PASS. All " << ntests << " tests passed." << color::reset << std::endl;
        }
    } else {
        std::cout << "Compiling file " << filename << "..." << std::endl;
        if (! compile(filename, /* test */nullptr))
            return EXIT_FAILURE;
    }
   return EXIT_SUCCESS;
}
