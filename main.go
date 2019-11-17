package main

import (
	"github.com/hajimehoshi/ebiten"
	"github.com/hajimehoshi/ebiten/ebitenutil"
)

func update(screen *ebiten.Image) error {
	ebitenutil.DebugPrint(screen, "Our first game in Ebiten!")
	return nil
}

func main() {
	// The original implementation of the Chip-8 language used a 64x32-pixel monochrome display.
	if err := ebiten.Run(update, 64*3, 32*3, 2, "Hello world!"); err != nil {
		panic(err)
	}
}
