// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.

// max_addr = KBD - 1
@KBD
D=A
@max_addr
M=D-1

// addr = SCREEN
@SCREEN
D=A
@addr
M=D

// curr_color = 1
@curr_color
M=0

@LOOP
0; JMP

(LOOP)
  @KBD
  D=M
  @WHITEN
  D; JEQ
  @BLACKEN
  0; JMP

(WHITEN)
  @curr_color
  D=M
  M=0
  @UPDATE_EXISTING
  D; JEQ
  @UPDATE_NEW
  0; JMP

(BLACKEN)
  @curr_color
  D=M
  M=-1
  @UPDATE_NEW
  D; JEQ
  @UPDATE_EXISTING
  0; JMP

(UPDATE_NEW)
  // Update the initial screen register
  @curr_color
  D=M
  @SCREEN
  M=D
  // Save the next addr
  D=A
  @addr
  M=D+1
  // Go back to the main loop
  @LOOP
  0; JMP
  
(UPDATE_EXISTING)
  // If addr > max_addr,
  // then go back to the main loop
  @addr
  D=M
  @max_addr
  D=M-D
  @LOOP
  D; JLT
  // Update addr's screen register
  @curr_color
  D=M
  @addr
  A=M
  M=D
  // addr++
  @addr
  M=M+1
  // Return to the main loop
  @LOOP
  0; JMP
