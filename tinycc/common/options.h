#pragma once

#include <cstdlib>
#include <cstring>
#include <iostream>
#include "colors.h"

namespace tiny {
    
    class Options {
    public:
        static inline bool verboseAST = false;
        static inline bool verboseIL = true;
        static inline bool rawAST = false;
        static inline bool exitAfterFailure = true;
        static inline bool testIR = true;

        static void setVerbose() {
            verboseAST = true;
            verboseIL = true;
        }

        static int parseArgs(int argc, char * argv[], char const * & filename) {
            using namespace colors;
            for (int i = 1; i < argc; ++i) {
                if (strcmp(argv[i], "--verbose") == 0) {
                    setVerbose();
                } else if (strcmp(argv[i], "--verboseAST") == 0) {
                    verboseAST = true;
                } else if (strcmp(argv[i], "--rawAST") == 0) {
                    rawAST = true;
                } else if (strcmp(argv[i], "--verboseIL") == 0) {
                    verboseIL = true;
                } else if (strcmp(argv[i], "--exitAfterFailure") == 0) {
                    exitAfterFailure = true;
                } else if (filename == nullptr) {
                    filename = argv[i];
                } else {
                    std::cerr << color::red << "ERROR: Invalid argument " << argv[i] << color::reset << std::endl;
                    return EXIT_FAILURE;
                }
            }
            return EXIT_SUCCESS;
        }
    }; // tiny::Options

} // namespace tiny