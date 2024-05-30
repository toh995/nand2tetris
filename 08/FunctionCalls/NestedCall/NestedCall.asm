@256
D=A
@SP
M=D
// call Sys.init 0
// Save the current stack frame
@Sys.init_RETURN2
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@5
D=D-A
@ARG
M=D
// Manage the program jumps
@Sys.init
0;JMP
(Sys.init_RETURN2)
// function Sys.init 0
(Sys.init)
@SP
D=M
@LCL
M=D
@SP
M=D
// push constant 4000
@4000
D=A
@SP
M=M+1
A=M-1
M=D
// pop pointer 0
@SP
M=M-1
A=M
D=M
@THIS
M=D
// push constant 5000
@5000
D=A
@SP
M=M+1
A=M-1
M=D
// pop pointer 1
@SP
M=M-1
A=M
D=M
@THAT
M=D
// call Sys.main 0
// Save the current stack frame
@Sys.main_RETURN0
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@5
D=D-A
@ARG
M=D
// Manage the program jumps
@Sys.main
0;JMP
(Sys.main_RETURN0)
// pop temp 1
@SP
M=M-1
A=M
D=M
@6
M=D
// label LOOP
(LOOP)
// goto LOOP
@LOOP
0;JMP
// function Sys.main 5
(Sys.main)
@SP
D=M
@LCL
M=D
A=D
M=0
D=D+1
A=D
M=0
D=D+1
A=D
M=0
D=D+1
A=D
M=0
D=D+1
A=D
M=0
D=D+1
@SP
M=D
// push constant 4001
@4001
D=A
@SP
M=M+1
A=M-1
M=D
// pop pointer 0
@SP
M=M-1
A=M
D=M
@THIS
M=D
// push constant 5001
@5001
D=A
@SP
M=M+1
A=M-1
M=D
// pop pointer 1
@SP
M=M-1
A=M
D=M
@THAT
M=D
// push constant 200
@200
D=A
@SP
M=M+1
A=M-1
M=D
// pop local 1
@1
D=A
@LCL
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// push constant 40
@40
D=A
@SP
M=M+1
A=M-1
M=D
// pop local 2
@2
D=A
@LCL
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// push constant 6
@6
D=A
@SP
M=M+1
A=M-1
M=D
// pop local 3
@3
D=A
@LCL
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
// push constant 123
@123
D=A
@SP
M=M+1
A=M-1
M=D
// call Sys.add12 1
// Save the current stack frame
@Sys.add12_RETURN1
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@6
D=D-A
@ARG
M=D
// Manage the program jumps
@Sys.add12
0;JMP
(Sys.add12_RETURN1)
// pop temp 0
@SP
M=M-1
A=M
D=M
@5
M=D
// push local 0
@0
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// push local 1
@1
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// push local 2
@2
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// push local 3
@3
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// push local 4
@4
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// return
// Store the return address for later usage
@LCL
D=M
@5
D=D-A
A=D
D=M
@R13
M=D
// Copy the return value onto argument 0
@SP
A=M-1
D=M
@ARG
A=M
M=D
// Reset @SP
D=A+1
@SP
M=D
//Restore the caller's segment pointers
@LCL
D=M
@R14
M=D
@R14
M=M-1
A=M
D=M
@THAT
M=D
@R14
M=M-1
A=M
D=M
@THIS
M=D
@R14
M=M-1
A=M
D=M
@ARG
M=D
@R14
M=M-1
A=M
D=M
@LCL
M=D
// Jump to the return address
@R13
A=M
0;JMP
// function Sys.add12 0
(Sys.add12)
@SP
D=M
@LCL
M=D
@SP
M=D
// push constant 4002
@4002
D=A
@SP
M=M+1
A=M-1
M=D
// pop pointer 0
@SP
M=M-1
A=M
D=M
@THIS
M=D
// push constant 5002
@5002
D=A
@SP
M=M+1
A=M-1
M=D
// pop pointer 1
@SP
M=M-1
A=M
D=M
@THAT
M=D
// push argument 0
@0
D=A
@ARG
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// push constant 12
@12
D=A
@SP
M=M+1
A=M-1
M=D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// return
// Store the return address for later usage
@LCL
D=M
@5
D=D-A
A=D
D=M
@R13
M=D
// Copy the return value onto argument 0
@SP
A=M-1
D=M
@ARG
A=M
M=D
// Reset @SP
D=A+1
@SP
M=D
//Restore the caller's segment pointers
@LCL
D=M
@R14
M=D
@R14
M=M-1
A=M
D=M
@THAT
M=D
@R14
M=M-1
A=M
D=M
@THIS
M=D
@R14
M=M-1
A=M
D=M
@ARG
M=D
@R14
M=M-1
A=M
D=M
@LCL
M=D
// Jump to the return address
@R13
A=M
0;JMP