#pragma once

#include "core/array.h"
#include "token.h"

struct AstRoot;

void parse_file(AstRoot *root, Array<Token> *tokens);
