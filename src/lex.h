#pragma once

#include "core/array.h"
#include "token.h"

Array<Token> lex_file(char *path, char *source);
