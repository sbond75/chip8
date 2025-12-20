/* Core of the CHIP-8 interpreter – 3-byte opcodes, 64 KiB RAM. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>
#include <assert.h>

#include "chip8.h"

static unsigned int quirks = QUIRKS_DEFAULT;

void c8_set_quirks(unsigned int q)
{
    quirks = q;
}

unsigned int c8_get_quirks(void)
{
    return quirks;
}

/* Fonts must live below PROG_OFFSET (<= 0x1B0 historically) */
#define FONT_OFFSET  0x01B0
#define HFONT_OFFSET 0x0110

int c8_verbose = 0;

chip8_t C8;

/* Display memory: 128x64 bits => 1024 bytes */
static uint8_t pixels[1024];

static int yield_flag = 0;
static int borked = 0;

static int screen_updated;
static int hi_res;

/* Keypad buffer (bitset for 16 keys) */
static uint16_t keys;

/* HP48 flags for SuperChip Fx75/Fx85 */
static uint8_t hp48_flags[16];

/* Text output */
char c8_message_text[MAX_MESSAGE_TEXT];
static int _puts_default(const char* s) { return fputs(s, stdout); }
int (*c8_puts)(const char* s) = _puts_default;

/* RNG */
int (*c8_rand)(void) = rand;

/* SYS hook */
c8_sys_hook_t c8_sys_hook = NULL;

/* 4x5 font (legacy) */
static const uint8_t font[] = {
    /* 0 */ 0xF0,0x90,0x90,0x90,0xF0, /* 1 */ 0x20,0x60,0x20,0x20,0x70,
    /* 2 */ 0xF0,0x10,0xF0,0x80,0xF0, /* 3 */ 0xF0,0x10,0xF0,0x10,0xF0,
    /* 4 */ 0x90,0x90,0xF0,0x10,0x10, /* 5 */ 0xF0,0x80,0xF0,0x10,0xF0,
    /* 6 */ 0xF0,0x80,0xF0,0x90,0xF0, /* 7 */ 0xF0,0x10,0x20,0x40,0x40,
    /* 8 */ 0xF0,0x90,0xF0,0x90,0xF0, /* 9 */ 0xF0,0x90,0xF0,0x10,0xF0,
    /* A */ 0xF0,0x90,0xF0,0x90,0x90, /* B */ 0xE0,0x90,0xE0,0x90,0xE0,
    /* C */ 0xF0,0x80,0x80,0x80,0xF0, /* D */ 0xE0,0x90,0x90,0x90,0xE0,
    /* E */ 0xF0,0x80,0xF0,0x80,0xF0, /* F */ 0xF0,0x80,0xF0,0x80,0x80,
};

/* SuperChip 8x10 font */
static const uint8_t hfont[] = {
    /* 0 */ 0x7C,0x82,0x82,0x82,0x82,0x82,0x82,0x82,0x7C,0x00,
    /* 1 */ 0x08,0x18,0x38,0x08,0x08,0x08,0x08,0x08,0x3C,0x00,
    /* 2 */ 0x7C,0x82,0x02,0x02,0x04,0x18,0x20,0x40,0xFE,0x00,
    /* 3 */ 0x7C,0x82,0x02,0x02,0x3C,0x02,0x02,0x82,0x7C,0x00,
    /* 4 */ 0x84,0x84,0x84,0x84,0xFE,0x04,0x04,0x04,0x04,0x00,
    /* 5 */ 0xFE,0x80,0x80,0x80,0xFC,0x02,0x02,0x82,0x7C,0x00,
    /* 6 */ 0x7C,0x82,0x80,0x80,0xFC,0x82,0x82,0x82,0x7C,0x00,
    /* 7 */ 0xFE,0x02,0x04,0x08,0x10,0x20,0x20,0x20,0x20,0x00,
    /* 8 */ 0x7C,0x82,0x82,0x82,0x7C,0x82,0x82,0x82,0x7C,0x00,
    /* 9 */ 0x7C,0x82,0x82,0x82,0x7E,0x02,0x02,0x82,0x7C,0x00,
    /* A */ 0x10,0x28,0x44,0x82,0x82,0xFE,0x82,0x82,0x82,0x00,
    /* B */ 0xFC,0x82,0x82,0x82,0xFC,0x82,0x82,0x82,0xFC,0x00,
    /* C */ 0x7C,0x82,0x80,0x80,0x80,0x80,0x80,0x82,0x7C,0x00,
    /* D */ 0xFC,0x82,0x82,0x82,0x82,0x82,0x82,0x82,0xFC,0x00,
    /* E */ 0xFE,0x80,0x80,0x80,0xF8,0x80,0x80,0x80,0xFE,0x00,
    /* F */ 0xFE,0x80,0x80,0x80,0xF8,0x80,0x80,0x80,0x80,0x00,
};

void c8_reset(void)
{
    memset(C8.V, 0, sizeof C8.V);
    memset(C8.RAM, 0, sizeof C8.RAM);
    C8.PC = PROG_OFFSET;
    C8.I  = 0;
    C8.DT = 0;
    C8.ST = 0;
    C8.SP = 0;
    memset(C8.stack, 0, sizeof C8.stack);

    assert(FONT_OFFSET + sizeof font <= PROG_OFFSET);
    memcpy(C8.RAM + FONT_OFFSET, font, sizeof font);
    assert(HFONT_OFFSET + sizeof hfont <= FONT_OFFSET);
    memcpy(C8.RAM + HFONT_OFFSET, hfont, sizeof hfont);

    hi_res = 0;
    screen_updated = 0;
    yield_flag = 0;
    borked = 0;
}

/* Helpers to fetch fields from a 3-byte opcode */
static inline void c8_fetch3(uint16_t pc, uint8_t *b0, uint8_t *b1, uint8_t *b2)
{
    assert(pc <= (uint16_t)(TOTAL_RAM - INSTR_SIZE));
    *b0 = C8.RAM[pc + 0];
    *b1 = C8.RAM[pc + 1];
    *b2 = C8.RAM[pc + 2];
}

void c8_step(void)
{
    if (yield_flag || borked) {
        return;
    }

    if (C8.PC > (uint16_t)(TOTAL_RAM - INSTR_SIZE)) {
        borked = 1;
        return;
    }

    uint8_t b0, b1, b2;
    c8_fetch3(C8.PC, &b0, &b1, &b2);
    C8.PC += INSTR_SIZE;

    const uint8_t cls  = (uint8_t)(b0 & 0xF0);
    const uint8_t x    = (uint8_t)(b0 & 0x0F);
    const uint8_t y    = (uint8_t)((b1 >> 4) & 0x0F);
    const uint8_t sub4 = (uint8_t)(b1 & 0x0F);          /* legacy nibble (low nibble of 2nd byte) */
    const uint16_t kk16  = (uint16_t)((b1 << 8) | b2);  /* widened immediates */
    const uint16_t addr16 = kk16;                       /* for 0/1/2/A/B op families */

    int row, col;
    screen_updated = 0;

    switch (cls) {
        case 0x00: {
            /* 00E0 CLS, 00EE RET, 00FB/00FC/00FD/00FE/00FF; 00Cn SCD */
            if (b1 == 0xE0) {
                /* CLS */
                memset(pixels, 0, sizeof pixels);
                screen_updated = 1;
            } else if (b1 == 0xEE) {
                /* RET */
                if (C8.SP == 0) {
                    borked = 1;
                    return;
                }
                C8.PC = C8.stack[--C8.SP];
            } else if ((uint8_t)(b1 & 0xF0) == 0xC0) {
                /* SCD n – scroll down by n lines; now n is 8-bit b2 */
                c8_resolution(&col, &row);
                row--;
                col >>= 3;
                uint8_t n = b2;
                while (row - n >= 0) {
                    memcpy(pixels + row * col, pixels + (row - n) * col, (size_t)col);
                    row--;
                }
                memset(pixels, 0x0, (size_t)n * (size_t)col);
                screen_updated = 1;
            } else if (b1 == 0xFB) {
                /* SCR */
                c8_resolution(&col, &row);
                col >>= 3;
                for (uint8_t yy = 0; yy < (uint8_t)row; yy++) {
                    for (int xx = col - 1; xx > 0; xx--) {
                        pixels[yy * col + xx] = (uint8_t)((pixels[yy * col + xx] << 4) |
                                                          (pixels[yy * col + xx - 1] >> 4));
                    }
                    pixels[yy * col] <<= 4;
                }
                screen_updated = 1;
            } else if (b1 == 0xFC) {
                /* SCL */
                c8_resolution(&col, &row);
                col >>= 3;
                for (uint8_t yy = 0; yy < (uint8_t)row; yy++) {
                    for (int xx = 0; xx < col - 1; xx++) {
                        pixels[yy * col + xx] = (uint8_t)((pixels[yy * col + xx] >> 4) |
                                                          (pixels[yy * col + xx + 1] << 4));
                    }
                    pixels[yy * col + (col - 1)] >>= 4;
                }
                screen_updated = 1;
            } else if (b1 == 0xFD) {
                /* EXIT */
                C8.PC -= INSTR_SIZE; /* Point back to 00FD */
                return;
            } else if (b1 == 0xFE) {
                /* LOW */
                if (hi_res) {
                    screen_updated = 1;
                }
                hi_res = 0;
            } else if (b1 == 0xFF) {
                /* HIGH */
                if (!hi_res) {
                    screen_updated = 1;
                }
                hi_res = 1;
            } else {
                /* SYS addr16 (no-op unless hook is installed) */
                if (c8_sys_hook) {
                    int ok = c8_sys_hook(addr16);
                    if (!ok) {
                        borked = 1;
                    }
                }
            }
        } break;

        case 0x10: /* JP addr16 */
            C8.PC = addr16;
            break;

        case 0x20: /* CALL addr16 */
            if (C8.SP >= 16) {
                return;
            }
            C8.stack[C8.SP++] = C8.PC;
            C8.PC = addr16;
            break;

        case 0x30: /* SE Vx, kk16 (low 8 bits used) */
            if (C8.V[x] == (uint8_t)kk16) {
                C8.PC += INSTR_SIZE;
            }
            break;

        case 0x40: /* SNE Vx, kk16 (low 8 bits used) */
            if (C8.V[x] != (uint8_t)kk16) {
                C8.PC += INSTR_SIZE;
            }
            break;

        case 0x50: /* SE Vx, Vy (low nibble of b1 must be 0) */
            if (sub4 == 0x0) {
                if (C8.V[x] == C8.V[y]) {
                    C8.PC += INSTR_SIZE;
                }
            }
            break;

        case 0x60: /* LD Vx, kk16 (low 8 bits) */
            C8.V[x] = (uint8_t)kk16;
            break;

        case 0x70: /* ADD Vx, kk16 (low 8 bits) */
            C8.V[x] = (uint8_t)(C8.V[x] + (uint8_t)kk16);
            break;

        case 0x80: { /* 8xy? – sub-op in low nibble of b1 (sub4) */
            uint16_t ans;
            uint8_t carry;
            switch (sub4) {
                case 0x0: /* LD Vx, Vy */
                    C8.V[x] = C8.V[y];
                    break;
                case 0x1: /* OR Vx, Vy */
                    C8.V[x] |= C8.V[y];
                    if (quirks & QUIRKS_VF_RESET) {
                        C8.V[0xF] = 0;
                    }
                    break;
                case 0x2: /* AND Vx, Vy */
                    C8.V[x] &= C8.V[y];
                    if (quirks & QUIRKS_VF_RESET) {
                        C8.V[0xF] = 0;
                    }
                    break;
                case 0x3: /* XOR Vx, Vy */
                    C8.V[x] ^= C8.V[y];
                    if (quirks & QUIRKS_VF_RESET) {
                        C8.V[0xF] = 0;
                    }
                    break;
                case 0x4: /* ADD Vx, Vy */
                    ans = (uint16_t)C8.V[x] + (uint16_t)C8.V[y];
                    C8.V[x] = (uint8_t)(ans & 0xFF);
                    C8.V[0xF] = (uint8_t)(ans > 255);
                    break;
                case 0x5: /* SUB Vx, Vy */
                    carry = (uint8_t)(C8.V[x] > C8.V[y]);
                    C8.V[x] = (uint8_t)((C8.V[x] - C8.V[y]) & 0xFF);
                    C8.V[0xF] = carry;
                    break;
                case 0x6: /* SHR Vx {, Vy} */
                    if (!(quirks & QUIRKS_SHIFT)) {
                        C8.V[x] = C8.V[y];
                    }
                    carry = (uint8_t)(C8.V[x] & 0x01);
                    C8.V[x] >>= 1;
                    C8.V[0xF] = carry;
                    break;
                case 0x7: /* SUBN Vx, Vy */
                    carry = (uint8_t)(C8.V[y] > C8.V[x]);
                    C8.V[x] = (uint8_t)((C8.V[y] - C8.V[x]) & 0xFF);
                    C8.V[0xF] = carry;
                    break;
                case 0xE: /* SHL Vx {, Vy} */
                    if (!(quirks & QUIRKS_SHIFT)) {
                        C8.V[x] = C8.V[y];
                    }
                    carry = (uint8_t)((C8.V[x] & 0x80) != 0);
                    C8.V[x] <<= 1;
                    C8.V[0xF] = carry;
                    break;
                default:
                    /* ignore other subcodes */
                    break;
            }
        } break;

        case 0x90: /* SNE Vx, Vy (sub4 must be 0) */
            if (sub4 == 0x0) {
                if (C8.V[x] != C8.V[y]) {
                    C8.PC += INSTR_SIZE;
                }
            }
            break;

        case 0xA0: /* LD I, addr16 */
            C8.I = addr16;
            break;

        case 0xB0: /* JP V0/ Vx, addr16 */
            if (quirks & QUIRKS_JUMP) {
                C8.PC = (uint16_t)(addr16 + C8.V[x]);
            } else {
                C8.PC = (uint16_t)(addr16 + C8.V[0]);
            }
            break;

        case 0xC0: /* RND Vx, kk16 (low 8 bits) */
            C8.V[x] = (uint8_t)(c8_rand() & (uint8_t)kk16);
            break;

        case 0xD0: { /* DRW Vx, Vy, height */
            int mW, mH, W, H, p, q;
            int tx, ty, byte, bit, pix;

            if (hi_res) {
                W = 128; H = 64; mW = 0x7F; mH = 0x3F;
            } else {
                W = 64; H = 32; mW = 0x3F; mH = 0x1F;
            }

            C8.V[0xF] = 0;

            /* Extended height: use b2 if non-zero, else legacy nibble */
            uint8_t height = (b2 != 0) ? b2 : sub4;

            if (height != 0) {
                /* 8x(height) sprite */
                uint8_t vx = C8.V[x], vy = C8.V[y];
                vx &= (uint8_t)mW;
                vy &= (uint8_t)mH;

                for (q = 0; q < height; q++) {
                    ty = vy + q;
                    if ((quirks & QUIRKS_CLIPPING) && (ty >= H)) {
                        break;
                    }
                    for (p = 0; p < 8; p++) {
                        tx = vx + p;
                        if ((quirks & QUIRKS_CLIPPING) && (tx >= W)) {
                            break;
                        }
                        pix = (C8.RAM[C8.I + q] & (0x80 >> p)) != 0;
                        if (pix) {
                            tx &= mW;
                            ty &= mH;
                            byte = ty * W + tx;
                            bit = 1 << (byte & 0x07);
                            byte >>= 3;
                            if (pixels[byte] & bit) {
                                C8.V[0x0F] = 1;
                            }
                            pixels[byte] ^= bit;
                        }
                    }
                }
            } else {
                /* SCHIP 16x16 sprite (legacy when height == 0) */
                uint8_t vx = C8.V[x], vy = C8.V[y];
                vx &= (uint8_t)mW;
                vy &= (uint8_t)mH;

                for (q = 0; q < 16; q++) {
                    ty = vy + q;
                    if ((quirks & QUIRKS_CLIPPING) && (ty >= H)) {
                        break;
                    }
                    for (p = 0; p < 16; p++) {
                        tx = vx + p;
                        if ((quirks & QUIRKS_CLIPPING) && (tx >= W)) {
                            break;
                        }
                        if (p >= 8) {
                            pix = (C8.RAM[C8.I + (q * 2) + 1] & (0x80 >> (p & 0x07))) != 0;
                        } else {
                            pix = (C8.RAM[C8.I + (q * 2)] & (0x80 >> p)) != 0;
                        }
                        if (pix) {
                            byte = ty * W + tx;
                            bit = 1 << (byte & 0x07);
                            byte >>= 3;
                            if (pixels[byte] & bit) {
                                C8.V[0x0F] = 1;
                            }
                            pixels[byte] ^= bit;
                        }
                    }
                }
            }

            screen_updated = 1;
            if (quirks & QUIRKS_DISP_WAIT) {
                yield_flag = 1;
            }
        } break;

        case 0xE0: {
            /* Use second byte as legacy kk subcode: 9E / A1 */
            if (b1 == 0x9E) {
                if (keys & (1u << C8.V[x])) {
                    C8.PC += INSTR_SIZE;
                }
            } else if (b1 == 0xA1) {
                if (!(keys & (1u << C8.V[x]))) {
                    C8.PC += INSTR_SIZE;
                }
            }
        } break;

        case 0xF0: {
            /* Use second byte as subcode – third byte ignored (legacy-compatible) */
            switch (b1) {
                case 0x07: /* LD Vx, DT */
                    C8.V[x] = C8.DT;
                    break;
                case 0x0A: /* LD Vx, K */
                    if (!keys) {
                        C8.PC -= INSTR_SIZE; /* repeat this instruction */
                        return;
                    }
                    for (uint8_t ky = 0; ky < 0x10; ky++) {
                        if (keys & (1u << ky)) {
                            C8.V[x] = ky;
                            break;
                        }
                    }
                    keys = 0;
                    break;
                case 0x15: /* LD DT, Vx */
                    C8.DT = C8.V[x];
                    break;
                case 0x18: /* LD ST, Vx */
                    C8.ST = C8.V[x];
                    break;
                case 0x1E: { /* ADD I, Vx (now 16-bit address space) */
                    uint32_t sum = (uint32_t)C8.I + (uint32_t)C8.V[x];
                    C8.I = (uint16_t)(sum & 0xFFFF);
                    C8.V[0xF] = (uint8_t)(sum > 0xFFFF);
                } break;
                case 0x29: /* LD F, Vx */
                    C8.I = (uint16_t)(FONT_OFFSET + (C8.V[x] & 0x0F) * 5u);
                    break;
                case 0x30: /* LD HF, Vx */
                    C8.I = (uint16_t)(HFONT_OFFSET + (C8.V[x] & 0x0F) * 10u);
                    break;
                case 0x33: /* LD B, Vx */
                    C8.RAM[C8.I]     = (uint8_t)((C8.V[x] / 100) % 10);
                    C8.RAM[C8.I + 1] = (uint8_t)((C8.V[x] / 10) % 10);
                    C8.RAM[C8.I + 2] = (uint8_t)(C8.V[x] % 10);
                    break;
                case 0x55: { /* LD [I], Vx */
                    size_t count = (size_t)x + 1u;
                    size_t avail = (C8.I < TOTAL_RAM) ? (size_t)(TOTAL_RAM - C8.I) : 0u;
                    if (count > avail) {
                        count = avail;
                    }
                    if (count > 0) {
                        memcpy(C8.RAM + C8.I, C8.V, count);
                    }
                    if (quirks & QUIRKS_MEM_CHIP8) {
                        C8.I = (uint16_t)(C8.I + count);
                    }
                } break;
                case 0x65: { /* LD Vx, [I] */
                    size_t count = (size_t)x + 1u;
                    size_t avail = (C8.I < TOTAL_RAM) ? (size_t)(TOTAL_RAM - C8.I) : 0u;
                    if (count > avail) {
                        count = avail;
                    }
                    if (count > 0) {
                        memcpy(C8.V, C8.RAM + C8.I, count);
                    }
                    if (quirks & QUIRKS_MEM_CHIP8) {
                        C8.I = (uint16_t)(C8.I + count);
                    }
                } break;
                case 0x75: /* LD R, Vx */
                    assert((size_t)x <= sizeof hp48_flags);
                    if (x > 0) {
                        memcpy(hp48_flags, C8.V, (size_t)x);
                    }
                    break;
                case 0x85: /* LD Vx, R */
                    assert((size_t)x <= sizeof hp48_flags);
                    if (x > 0) {
                        memcpy(C8.V, hp48_flags, (size_t)x);
                    }
                    break;
                default:
                    break;
            }
        } break;

        default:
            /* Unknown class – ignore */
            break;
    }
}

int c8_ended(void)
{
    if (borked) {
        return 1;
    }
    uint32_t op = c8_opcode(C8.PC);
    uint8_t b0 = (uint8_t)((op >> 16) & 0xFF);
    uint8_t b1 = (uint8_t)((op >> 8) & 0xFF);
    return (b0 == 0x00 && b1 == 0xFD);
}

int c8_waitkey(void)
{
    uint32_t op = c8_opcode(C8.PC);
    uint8_t b0 = (uint8_t)((op >> 16) & 0xFF);
    uint8_t b1 = (uint8_t)((op >> 8) & 0xFF);
    return ((b0 & 0xF0) == 0xF0) && (b1 == 0x0A);
}

uint8_t c8_get(uint16_t addr)
{
    assert(addr < TOTAL_RAM);
    return C8.RAM[addr];
}

void c8_set(uint16_t addr, uint8_t byte)
{
    assert(addr < TOTAL_RAM);
    C8.RAM[addr] = byte;
}

uint32_t c8_opcode(uint16_t addr)
{
    assert(addr <= (uint16_t)(TOTAL_RAM - INSTR_SIZE));
    return ((uint32_t)C8.RAM[addr] << 16) |
           ((uint32_t)C8.RAM[addr + 1] << 8) |
           (uint32_t)C8.RAM[addr + 2];
}

uint16_t c8_get_pc(void)
{
    return C8.PC;
}

uint16_t c8_prog_size(void)
{
    uint16_t n = TOTAL_RAM - 1;
    while (n > PROG_OFFSET && C8.RAM[n] == 0) {
        n--;
    }
    n++; /* first byte after last non-zero */
    /* Round up to next multiple of 3 to keep instructions aligned */
    uint16_t rem = (uint16_t)(n % INSTR_SIZE);
    if (rem != 0) {
        n = (uint16_t)(n + (INSTR_SIZE - rem));
    }
    return n;
}

uint8_t c8_get_reg(uint8_t r)
{
    if (r > 0xF) {
        return 0;
    }
    return C8.V[r];
}

int c8_screen_updated(void)
{
    return screen_updated;
}

int c8_resolution(int *w, int *h)
{
    if (!w || !h) {
        return hi_res;
    }
    if (hi_res) {
        *w = 128; *h = 64;
    } else {
        *w = 64; *h = 32;
    }
    return hi_res;
}

int c8_get_pixel(int x, int y)
{
    int w = hi_res ? 128 : 64;
    int h = hi_res ? 64 : 32;
    if (x < 0 || x >= w || y < 0 || y >= h) {
        return 0;
    }
    int idx = y * w + x;
    int bit = idx & 0x07;
    int byte = idx >> 3;
    assert((size_t)byte < sizeof pixels);
    return (pixels[byte] & (1 << bit)) != 0;
}

void c8_key_down(uint8_t k)
{
    if (k > 0xF) {
        return;
    }
    keys |= (uint16_t)(1u << k);
}

void c8_key_up(uint8_t k)
{
    if (k > 0xF) {
        return;
    }
    keys &= (uint16_t)~(1u << k);
}

void c8_60hz_tick(void)
{
    yield_flag = 0;
    if (C8.DT > 0) {
        C8.DT--;
    }
    if (C8.ST > 0) {
        C8.ST--;
    }
}

int c8_sound(void)
{
    return C8.ST > 0;
}

size_t c8_load_program(uint8_t program[], size_t n)
{
    if (n + PROG_OFFSET > TOTAL_RAM) {
        n = TOTAL_RAM - PROG_OFFSET;
    }
    assert(n + PROG_OFFSET <= TOTAL_RAM);
    memcpy(C8.RAM + PROG_OFFSET, program, n);
    return n;
}

int c8_load_file(const char *fname)
{
    FILE *f = fopen(fname, "rb");
    if (!f) {
        return 0;
    }

    fseek(f, 0, SEEK_END);
    long lenL = ftell(f);
    if (lenL <= 0) {
        fclose(f);
        return 0;
    }
    size_t len = (size_t)lenL;

    if (len + PROG_OFFSET > TOTAL_RAM) {
        fclose(f);
        return 0;
    }

    rewind(f);
    size_t r = fread(C8.RAM + PROG_OFFSET, 1, len, f);
    fclose(f);
    if (r != len) {
        return 0;
    }
    return (int)len;
}

char *c8_load_txt(const char *fname)
{
    FILE *f = fopen(fname, "rb");
    if (!f) {
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    long lenL = ftell(f);
    rewind(f);

    if (lenL < 0) {
        fclose(f);
        return NULL;
    }

    size_t len = (size_t)lenL;
    char *bytes = (char*)malloc(len + 2);
    if (!bytes) {
        fclose(f);
        return NULL;
    }

    size_t r = fread(bytes, 1, len, f);
    fclose(f);
    if (r != len) {
        free(bytes);
        return NULL;
    }

    bytes[len] = '\0';
    return bytes;
}

int c8_save_file(const char *fname)
{
    uint16_t n = c8_prog_size();
    size_t len = (size_t)(n - PROG_OFFSET);
    FILE *f = fopen(fname, "wb");
    if (!f) {
        return 0;
    }
    size_t w = fwrite(C8.RAM + PROG_OFFSET, 1, len, f);
    fclose(f);
    if (w != len) {
        return 0;
    }
    return (int)len;
}

int c8_message(const char *msg, ...)
{
    if (msg) {
        va_list arg;
        va_start(arg, msg);
        vsnprintf(c8_message_text, MAX_MESSAGE_TEXT - 1, msg, arg);
        va_end(arg);
    }
    if (c8_puts) {
        return c8_puts(c8_message_text);
    }
    return 0;
}

/* c8_include_callback is defined by the assembler user; default provided elsewhere */
