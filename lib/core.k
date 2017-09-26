extern fn bit_and(a u64, b u64) -> u64;
extern fn bit_or(a u64, b u64) -> u64;
extern fn bit_shift_left(x u64, amount u64) -> u64;
extern fn bit_shift_right(x u64, amount u64) -> u64;

extern fn malloc(bytes u64) -> *c_void;
extern fn free(mem *c_void);

extern fn dll_load(path *u8) -> *c_void;
extern fn dll_unload(dll *c_void);
extern fn dll_get_symbol(dll *c_void, symbol *u8) -> *c_void;
