package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"math/big"
	"math/rand"
	"os"
	"time"

	cryptorand "crypto/rand"
)

// stolen from: https://github.com/docker/cli/blob/aaa7a7cb9567cb5ed2e82facc2bbdd8a85347512/vendor/github.com/docker/docker/pkg/stringid/stringid.go#L81-L93
func init() {
	// safely set the seed globally so we generate random ids. Tries to use a
	// crypto seed before falling back to time.
	var seed int64
	if cryptoseed, err := cryptorand.Int(cryptorand.Reader, big.NewInt(math.MaxInt64)); err != nil {
		// This should not happen, but worst-case fallback to time-based seed.
		seed = time.Now().UnixNano()
	} else {
		seed = cryptoseed.Int64()
	}

	rand.Seed(seed)
}

// spec: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM

// The data should be stored in the interpreter area of Chip-8 memory (0x000 to 0x1FF).
// Example: "0"
// +------------------------+
// | **** | 11110000 | 0xF0 |
// | *  * | 10010000 | 0x90 |
// | *  * | 10010000 | 0x90 |
// | *  * | 10010000 | 0x90 |
// | **** | 11110000 | 0xF0 |
// +------------------------+
var fontsets = []byte{
	0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
	0x20, 0x60, 0x20, 0x20, 0x70, // 1
	0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
	0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
	0x90, 0x90, 0xF0, 0x10, 0x10, // 4
	0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
	0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
	0xF0, 0x10, 0x20, 0x40, 0x40, // 7
	0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
	0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
	0xF0, 0x90, 0xF0, 0x90, 0x90, // A
	0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
	0xF0, 0x80, 0x80, 0x80, 0xF0, // C
	0xE0, 0x90, 0x90, 0x90, 0xE0, // D
	0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
	0xF0, 0x80, 0xF0, 0x80, 0x80, // F
}

type vm struct {
	// The Chip 8 has 4K memory in total.
	//
	//
	// 0x000 - 0x1FF - Chip 8 interpreter (contains font set in emu)
	// 0x050 - 0x0A0 - Used for the built in 4x5 pixel font set (0-F)
	// 0x200 - 0xFFF - Program ROM and work RAM
	//
	// +---------------+= 0xFFF (4095) End of Chip-8 RAM
	// |               |
	// |               |
	// |               |
	// |               |
	// |               |
	// | 0x200 to 0xFFF|
	// |     Chip-8    |
	// | Program / Data|
	// |     Space     |
	// |               |
	// |               |
	// |               |
	// +- - - - - - - -+= 0x600 (1536) Start of ETI 660 Chip-8 programs
	// |               |
	// |               |
	// |               |
	// +---------------+= 0x200 (512) Start of most Chip-8 programs
	// | 0x000 to 0x1FF|
	// | Reserved for  |
	// |  interpreter  |
	// +---------------+= 0x000 (0) Start of Chip-8 RAM
	memory [4096]byte

	// CPU v: The Chip 8 has 15 8-bit general purpose v
	// named V0,V1 up to VE. The 16th register is used for the ‘carry flag’.
	// Eight bits is one byte so we can use an unsigned char for this purpose.
	v [16]byte

	// program counter has a value from 0x000 to 0xFFF
	pc uint16

	// index register has a value from 0x000 to 0xFFF
	ir uint16

	// Interupts and hardware registers.
	// The Chip 8 has none, but there are two timer registers that count at 60 Hz.
	// When set above zero they will count down to zero.
	// ・The system’s buzzer sounds whenever the sound timer reaches zero.
	delayTimer, soundTimer uint8

	stack [16]uint16

	// stack pointer
	sp uint16

	// The Chip 8 has a HEX based keypad 0x0-0xF.
	// This is current state of the key.
	key [16]byte

	// The original implementation of the Chip-8 language used a 64x32-pixel
	// monochrome display.
	display [64 * 32]byte

	// debug mode
	debug bool
}

func (vm *vm) String() string {
	return fmt.Sprintf("[PC: %d, SP: %d]", vm.pc, vm.sp)
}

func new() *vm {
	vm := &vm{
		pc: 0x200, // Program counter starts at 0x200
	}
	copy(vm.memory[:], fontsets)
	return vm
}

func (vm *vm) loadRom(rom []byte) error {
	if len(rom) > 0xFFF-0x200 {
		return errors.New("rom is larger than program / data space")
	}

	copy(vm.memory[0x200:], rom)

	return nil
}

func (vm *vm) ReadRomFile(path string) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()

	rom, err := ioutil.ReadAll(f)
	if err != nil {
		return err
	}

	return vm.loadRom(rom)
}

func (vm *vm) fetchOpCode() uint16 {
	// To demonstrate how this works we will be using opcode `0xA2F0`.
	// The following:
	// memory[pc]     == 0xA2
	// memory[pc + 1] == 0xF0
	//
	// 	   0xA2   0xA2 << 8 = 0xA200   HEX
	// 10100010   1010001000000000     BIN
	first := uint16(vm.memory[vm.pc]) << 8
	second := uint16(vm.memory[vm.pc+1])

	// 	1010001000000000   // 0xA200
	//  |      11110000    // 0xF0 (0x00F0)
	// ------------------
	// 1010001011110000    // 0xA2F0
	return first | second
}

// VX == vm.v[x]
// VY == vm.v[y]
// VF == vm.v[0xF]
func (vm *vm) step() error {
	opcode := vm.fetchOpCode()
	switch opcode & 0xF000 {
	case 0x0000:
		switch opcode & 0x00FF {
		case 0x00E0:
			vm.cls()
		case 0x00EE:
			vm.ret()
		default:
			vm.sys(opcode & 0x0FFF)
		}
	case 0x1000:
		vm.jump(opcode & 0x0FFF)
	case 0x2000:
		vm.call(opcode & 0x0FFF)
	case 0x3000:
		x := (opcode & 0x0F00) >> 8
		nn := opcode & 0x00FF
		vm.skipIf(x, byte(nn))
	case 0x4000:
		x := (opcode & 0x0F00) >> 8
		nn := opcode & 0x00FF
		vm.skipIfNot(x, byte(nn))
	case 0x5000:
		x := (opcode & 0x0F00) >> 8
		y := (opcode & 0x00F0) >> 4
		vm.skipIfXY(x, y)
	case 0x6000:
		x := (opcode & 0x0F00) >> 8
		nn := opcode & 0x00FF
		vm.loadX(x, byte(nn))
	case 0x7000:
		x := (opcode & 0x0F00) >> 8
		nn := opcode & 0x00FF
		vm.addX(x, byte(nn))
	case 0x8000:
		x := (opcode & 0x0F00) >> 8
		y := (opcode & 0x00F0) >> 4
		switch opcode & 0xF00F {
		case 0x8000:
			vm.loadXY(x, y)
		case 0x8001:
			vm.or(x, y)
		case 0x8002:
			vm.and(x, y)
		case 0x8003:
			vm.xor(x, y)
		case 0x8004:
			vm.add(x, y)
		case 0x8005:
			vm.sub(x, y)
		case 0x8006:
			vm.shiftr(x)
		case 0x8007:
			vm.subYX(x, y)
		case 0x800E:
			vm.shiftl(x)
		}
	case 0x9000:
		x := (opcode & 0x0F00) >> 8
		y := (opcode & 0x00F0) >> 4
		vm.skipIfNotXY(x, y)
	case 0xA000:
		vm.load(opcode & 0x0FFF)
	case 0xB000:
		vm.jumpV0(opcode & 0x0FFF)
	case 0xC000:
		x := (opcode & 0x0F00) >> 8
		nn := opcode & 0x00FF
		vm.loadRand(x, byte(nn))
	case 0xD000:
		x := (opcode & 0x0F00) >> 8
		y := (opcode & 0x00F0) >> 4
		n := opcode & 0x000F
		vm.draw(x, y, byte(n))
	case 0xE000:
		x := (opcode & 0x0F00) >> 8
		switch opcode & 0xF0FF {
		case 0xE09E:
			vm.skipIfKeyPressed(x)
		case 0xE0A1:
			vm.skipIfNotKeyPressed(x)
		}
	case 0xF000:
		x := (opcode & 0x0F00) >> 8
		switch opcode & 0x00FF {
		case 0xF007:
			vm.loadXdelay(x)
		case 0xF00A:
			vm.loadXKey(x)
		case 0xF015:
			vm.loadDelayX(x)
		case 0xF018:
			vm.loadSoundX(x)
		case 0xF01E:
			vm.addIX(x)
		case 0xF029:
			vm.loadIFont(x)
		case 0xF033:
			vm.bcd(x)
		case 0xF055:
			vm.regDump(x)
		case 0xF065:
			vm.regLoad(x)
		}
	default:
		panic(fmt.Sprintf("unknown opcode: 0x%X\n", opcode))
	}

	vm.pc += 2

	return nil
}

// cls clears the screen.
func (vm *vm) cls() {
	if vm.debug {
		log.Println("CLS")
	}
	for i := range vm.display {
		vm.display[i] = 0
	}
}

// ret returns from a subroutine.
func (vm *vm) ret() {
	if vm.debug {
		log.Printf("RET %s\n", vm)
	}
	vm.pc = vm.stack[vm.sp]
	vm.sp--
}

// sys calls RCA 1802 program at address NNN. Not necessary for most ROMs.
// https://en.wikipedia.org/wiki/RCA_1802
func (vm *vm) sys(addr uint16) {
	panic("SYS is unimplemented")
}

// jump jumps to address NNN.
func (vm *vm) jump(addr uint16) {
	if vm.debug {
		log.Printf("JP 0x%X, %s\n", addr, vm)
	}
	vm.pc = addr
}

// call calls a subroutine at address.
func (vm *vm) call(addr uint16) {
	if vm.debug {
		log.Printf("CALL 0x%X, %s\n", addr, vm)
	}
	if int(vm.sp) >= len(vm.stack) {
		panic("stack overflow!")
	}
	vm.stack[vm.sp] = vm.pc
	vm.sp++

	vm.pc = addr
}

// skipIf skips the next instruction if VX equals NN.
// (Usually the next instruction is a jump to skip a code block)
func (vm *vm) skipIf(x uint16, addr byte) {
	if vm.debug {
		log.Printf("SE V[0x%X], 0x%X, %s\n", x, addr, vm)
	}
	if vm.v[x] == addr {
		vm.pc += 2
	}
}

// skipIfNot skips the next instruction if VX doesn't equal NN.
// (Usually the next instruction is a jump to skip a code block)
func (vm *vm) skipIfNot(x uint16, addr byte) {
	if vm.debug {
		log.Printf("SNE V[0x%X], 0x%X, %s\n", x, addr, vm)
	}
	if vm.v[x] != addr {
		vm.pc += 2
	}
}

// skipIfXY skips the next instruction if VX equals VY.
// (Usually the next instruction is a jump to skip a code block)
func (vm *vm) skipIfXY(x, y uint16) {
	if vm.debug {
		log.Printf("SE V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	if vm.v[x] == vm.v[y] {
		vm.pc += 2
	}
}

// loadX sets NN to VX.
func (vm *vm) loadX(x uint16, addr byte) {
	if vm.debug {
		log.Printf("LD V[0x%X], 0x%X, %s\n", x, addr, vm)
	}
	vm.v[x] = addr
}

// addX adds NN to VX. (Carry flag is not changed)
func (vm *vm) addX(x uint16, addr byte) {
	if vm.debug {
		log.Printf("ADD V[0x%X], 0x%X, %s\n", x, addr, vm)
	}
	vm.v[x] += addr
}

// loadXY sets VX to the value of VY.
func (vm *vm) loadXY(x, y uint16) {
	if vm.debug {
		log.Printf("LD V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	vm.v[x] = vm.v[y]
}

// or sets VX to VX or VY. (Bitwise OR operation)
func (vm *vm) or(x, y uint16) {
	if vm.debug {
		log.Printf("OR V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	vm.v[x] |= vm.v[y]
}

// and sets VX to VX and VY. (Bitwise AND operation)
func (vm *vm) and(x, y uint16) {
	if vm.debug {
		log.Printf("AND V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	vm.v[x] &= vm.v[y]
}

// xor sets VX to VX xor VY. (Bitwise XOR operation)
func (vm *vm) xor(x, y uint16) {
	if vm.debug {
		log.Printf("XOR V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	vm.v[x] ^= vm.v[y]
}

// add adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
func (vm *vm) add(x, y uint16) {
	if vm.debug {
		log.Printf("ADD V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	vm.v[x] += vm.v[y]

	if vm.v[x] < vm.v[y] {
		vm.v[0xF] = 1 // carry flag
	} else {
		vm.v[0xF] = 0
	}
}

// sub VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
func (vm *vm) sub(x, y uint16) {
	if vm.debug {
		log.Printf("SUB V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	vm.v[x] -= vm.v[y]

	if vm.v[x] < vm.v[y] {
		vm.v[0xF] = 1 // carry flag
	} else {
		vm.v[0xF] = 0
	}
}

// shiftr stores the least significant bit of VX in VF and then shifts VX to the right by 1.
func (vm *vm) shiftr(x uint16) {
	if vm.debug {
		log.Printf("SHR V[0x%X], %s\n", x, vm)
	}
	vm.v[0xF] = vm.v[x] & 0x0001
	vm.v[x] >>= 1
}

// subYX sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
func (vm *vm) subYX(x, y uint16) {
	if vm.debug {
		log.Printf("SUBN V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	if vm.v[x] < vm.v[y] {
		vm.v[0xF] = 1 // carry flag
	} else {
		vm.v[0xF] = 0
	}

	vm.v[x] = vm.v[y] - vm.v[x]
}

// shiftl stores the most significant bit of VX in VF and then shifts VX to the left by 1.
func (vm *vm) shiftl(x uint16) {
	if vm.debug {
		log.Printf("SHL V[0x%X], %s\n", x, vm)
	}
	vm.v[0xF] = vm.v[x] & 0x0001
	vm.v[x] <<= 1
}

// skipIfNotXY skips the next instruction if VX doesn't equal VY.
// (Usually the next instruction is a jump to skip a code block)
func (vm *vm) skipIfNotXY(x, y uint16) {
	if vm.debug {
		log.Printf("SNE V[0x%X], V[0x%X], %s\n", x, y, vm)
	}
	if vm.v[x] != vm.v[y] {
		vm.pc += 2
	}
}

// load sets I to the address NNN.
func (vm *vm) load(addr uint16) {
	if vm.debug {
		log.Printf("LD I, 0x%X, %s\n", addr, vm)
	}
	vm.ir = addr
	vm.pc += 2
}

// jumpV0 jumps to the address NNN plus V0.
func (vm *vm) jumpV0(addr uint16) {
	if vm.debug {
		log.Printf("JP V0, 0x%X, %s\n", addr, vm)
	}
	vm.pc = addr + uint16(vm.v[0])
}

var random = rand.Intn

// loadRand sets VX to the result of a bitwise and operation on
// a random number (Typically: 0 to 255) and NN.
func (vm *vm) loadRand(x uint16, addr byte) {
	if vm.debug {
		log.Printf("RND V[0x%X], 0x%X, %s\n", x, addr, vm)
	}
	vm.v[x] = byte(random(256)) & addr
}

// draw draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels.
// Each row of 8 pixels is read as bit-coded starting from memory location I (index register);
// I value doesn’t change after the execution of this instruction. As described above, VF is set
// to 1 if any screen pixels are flipped from set to unset when the sprite is drawn, and
// to 0 if that doesn’t happen.
func (vm *vm) draw(x, y uint16, height byte) {
	if vm.debug {
		log.Printf("DRW V[0x%X], V[0x%X], height = 0x%X, %s\n", x, y, height, vm)
	}
	vm.v[0xF] = 0
	h := uint16(height)

	for yline := uint16(0); yline < h; y++ {
		pixel := vm.memory[vm.ir+yline]
		for xline := uint16(0); xline < 8; xline++ {
			idx := x + xline + ((y + yline) * 64)
			if (pixel & (0x80 >> xline)) != 0 {
				if vm.display[idx] == 1 {
					vm.v[0xF] = 1
				}
				vm.display[idx] ^= 1
			}
		}
	}
}

// skipIfKeyPressed skips the next instruction if the key stored in VX is pressed.
// (Usually the next instruction is a jump to skip a code block)
func (vm *vm) skipIfKeyPressed(x uint16) {
	if vm.debug {
		log.Printf("SKP V[0x%X], %s\n", x, vm)
	}
	if vm.key[vm.v[x]] == 1 {
		vm.pc += 2
	}
}

// skipIfNotKeyPressed skips the next instruction if the key stored in VX isn't pressed.
// (Usually the next instruction is a jump to skip a code block)
func (vm *vm) skipIfNotKeyPressed(x uint16) {
	if vm.debug {
		log.Printf("SKNP V[0x%X], %s\n", x, vm)
	}
	if vm.key[vm.v[x]] != 1 {
		vm.pc += 2
	}
}

// loadXdelay sets the value of the delay timer to VX.
func (vm *vm) loadXdelay(x uint16) {
	if vm.debug {
		log.Printf("LD V[0x%X], DT, %s\n", x, vm)
	}
	vm.v[x] = vm.delayTimer
}

// A key press is awaited, and then stored in VX.
// (Blocking Operation. All instruction halted until next key event)
func (vm *vm) loadXKey(x uint16) {
	reached := byte(0xFF)
	for _, v := range vm.key {
		if v == 1 {
			reached = v
		}
	}
	// Blocking.
	if reached == 0xFF {
		vm.pc -= 2
		return
	}
	vm.v[x] = reached
	if vm.debug {
		log.Printf("LD V[0x%X], K = 0x%X, %s\n", x, reached, vm)
	}
}

// loadDelayX sets VX to the delay timer.
func (vm *vm) loadDelayX(x uint16) {
	if vm.debug {
		log.Printf("LD DT, V[0x%X], %s\n", x, vm)
	}
	vm.delayTimer = vm.v[x]
}

// loadSoundX sets VX to the sound timer.
func (vm *vm) loadSoundX(x uint16) {
	if vm.debug {
		log.Printf("LD ST, V[0x%X], %s\n", x, vm)
	}
	vm.soundTimer = vm.v[x]
}

// addIX adds VX to I. VF is set to 1 when there is a range overflow (I+VX>0xFFF),
// and to 0 when there isn't.
func (vm *vm) addIX(x uint16) {
	if vm.debug {
		log.Printf("ADD I, V[0x%X], %s\n", x, vm)
	}
	vm.ir += x
	if vm.ir > 0xFFF {
		vm.v[0xF] = 1
	} else {
		vm.v[0xF] = 0
	}
}

// loadIFont sets I to the location of the sprite for the character in VX.
// Characters 0-F (in hexadecimal) are represented by a 4x5 font.
func (vm *vm) loadIFont(x uint16) {
	if vm.debug {
		log.Printf("LD F, V[0x%X], %s\n", x, vm)
	}
	vm.ir = uint16(vm.memory[vm.v[x]*0x0005])
}

// Stores the binary-coded decimal representation of VX, with the most significant
// of three digits at the address in I, the middle digit at I plus 1, and the least significant digit at
// I plus 2. (In other words, take the decimal representation of VX, place the hundreds digit in memory at
// location in I, the tens digit at location I+1, and the ones digit at location I+2.)
func (vm *vm) bcd(x uint16) {
	if vm.debug {
		log.Printf("LD B, V[0x%X], %s\n", x, vm)
	}
	vm.memory[vm.ir] = (vm.v[x] / 100) % 10
	vm.memory[vm.ir+1] = (vm.v[x] / 10) % 10
	vm.memory[vm.ir+2] = vm.v[x] % 10
}

// Stores V0 to VX (including VX) in memory starting at address I.
// The offset from I is increased by 1 for each value written, but I itself is left unmodified.
func (vm *vm) regDump(x uint16) {
	if vm.debug {
		log.Printf("LD [I], V[0x%X], %s\n", x, vm)
	}
	for i := uint16(0); i <= x; i++ {
		vm.memory[vm.ir] = vm.v[i]
		vm.ir++
	}
}

// Fills V0 to VX (including VX) with values from memory starting at address I. The offset from
// I is increased by 1 for each value written, but I itself is left unmodified.
func (vm *vm) regLoad(x uint16) {
	if vm.debug {
		log.Printf("LD V[0x%X], I, %s\n", x, vm)
	}
	for i := uint16(0); i <= x; i++ {
		vm.v[i] = vm.memory[vm.ir]
		vm.ir++
	}
}
