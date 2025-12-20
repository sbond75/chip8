/** chip8.h (3-byte opcodes, 64 KiB RAM)
 * Core public API – platform independent.
 */

#ifndef CHIP8_H
#define CHIP8_H

#include <stdint.h>
#include <stddef.h>

/** Maximum addressable memory */
#define TOTAL_RAM (16 * 4096)   /* 65536 bytes */

/** Offset where the program is loaded in RAM */
#define PROG_OFFSET 512

/** Internal message buffer size */
#define MAX_MESSAGE_TEXT 128

/** Size of one instruction (bytes) */
#define INSTR_SIZE 3

/** Interpreter state */
typedef struct {
    uint8_t  V[16];
    uint8_t  RAM[TOTAL_RAM];
    uint16_t PC;        /* Program counter (byte address; multiples of 3) */
    uint16_t I;         /* Index register (16-bit for 64 KiB) */
    uint8_t  DT, ST;    /* Delay / Sound timers */
    uint16_t stack[16];
    uint8_t  SP;
} chip8_t;

extern chip8_t C8;

/** Quirks flags */
#define QUIRKS_VF_RESET  0x01
#define QUIRKS_MEM_CHIP8 0x02
#define QUIRKS_DISP_WAIT 0x04
#define QUIRKS_CLIPPING  0x08
#define QUIRKS_SHIFT     0x10
#define QUIRKS_JUMP      0x20

#define QUIRKS_DEFAULT (QUIRKS_VF_RESET | QUIRKS_SHIFT | QUIRKS_CLIPPING)
#define QUIRKS_CHIP8   (QUIRKS_VF_RESET | QUIRKS_MEM_CHIP8 | QUIRKS_DISP_WAIT | QUIRKS_CLIPPING)
#define QUIRKS_SCHIP   (QUIRKS_CLIPPING | QUIRKS_SHIFT | QUIRKS_JUMP)

void         c8_set_quirks(unsigned int q);
unsigned int c8_get_quirks(void);

/** Verbose logging control */
extern int c8_verbose;

/** Interpreter core */
void c8_reset(void);
void c8_step(void);
int  c8_ended(void);
int  c8_waitkey(void);

/** SYS hook */
typedef int (*c8_sys_hook_t)(unsigned int nnn);
extern c8_sys_hook_t c8_sys_hook;

/** Debug helpers */
uint8_t   c8_get(uint16_t addr);
void      c8_set(uint16_t addr, uint8_t byte);

/* Returns a packed 24-bit opcode starting at addr: (b0<<16)|(b1<<8)|b2 */
uint32_t  c8_opcode(uint16_t addr);

uint16_t  c8_get_pc(void);
uint16_t  c8_prog_size(void);
uint8_t   c8_get_reg(uint8_t r);

/** RNG used by Cxkk (still 8-bit mask at the register) */
extern int (*c8_rand)(void);

/** Graphics */
int c8_screen_updated(void);
int c8_resolution(int *w, int *h);
int c8_get_pixel(int x, int y);

/** Keyboard */
void c8_key_down(uint8_t k);
void c8_key_up(uint8_t k);

/** Timers and sound */
void c8_60hz_tick(void);
int  c8_sound(void);

/** I/O helpers */
size_t c8_load_program(uint8_t program[], size_t n);
int    c8_load_file(const char *fname);
int    c8_save_file(const char *fname);
char  *c8_load_txt(const char *fname);

/** Output */
extern int  (*c8_puts)(const char* s);
int         c8_message(const char *msg, ...);
extern char c8_message_text[];

/** Assembler / Disassembler (signatures unchanged) */
int  c8_assemble(const char *text);

typedef char *(*c8_include_callback_t)(const char *fname);
extern c8_include_callback_t c8_include_callback;

void c8_disasm_start(void);
void c8_disasm_reachable(uint16_t addr);
void c8_disasm(void);

#endif /* CHIP8_H */
