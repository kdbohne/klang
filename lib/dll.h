#pragma once

void *dll_load(char *path);
void dll_unload(void *dll);
void *dll_get_symbol(void *dll, char *symbol);
