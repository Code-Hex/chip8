package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_new(t *testing.T) {
	vm := new()
	if vm.pc != 0x200 {
		t.Errorf("program counter (got %d, but want %d)", vm.pc, 0x200)
	}
	if len(vm.memory) != 4096 {
		t.Errorf("unexpected memory size")
	}
	dumpFontSets := vm.memory[:len(fontsets)]
	if diff := cmp.Diff(fontsets, dumpFontSets); diff != "" {
		t.Errorf("fontsets: (-want, +got)\n%s", diff)
	}
	if len(vm.registers) != 16 {
		t.Errorf("unexpected register size")
	}
	if len(vm.key) != 16 {
		t.Errorf("unexpected key size")
	}
	if len(vm.display) != 64*32 {
		t.Errorf("unexpected display size")
	}
}

func Test_vm_loadRom(t *testing.T) {
	tests := []struct {
		name    string
		rom     []byte
		wantErr bool
	}{
		{
			name:    "valid",
			rom:     make([]byte, 0xFFF-0x200),
			wantErr: false,
		},
		{
			name:    "invalid",
			rom:     make([]byte, 0xFFF-0x200+1),
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			vm := new()
			if err := vm.loadRom(tt.rom); (err != nil) != tt.wantErr {
				t.Errorf("vm.loadRom() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
