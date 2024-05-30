@256
D=A
@SP
M=D
// call Sys.init 0
// Save the current stack frame
@Sys.init_RETURN4
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
(Sys.init_RETURN4)
// function Class1.set 0
(Class1.set)
@SP
D=M
@LCL
M=D
@SP
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
// pop static 0
@SP
M=M-1
A=M
D=M
@Class1.0
M=D
// push argument 1
@1
D=A
@ARG
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// pop static 1
@SP
M=M-1
A=M
D=M
@Class1.1
M=D
// push constant 0
@0
D=A
@SP
M=M+1
A=M-1
M=D
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
// function Class1.get 0
(Class1.get)
@SP
D=M
@LCL
M=D
@SP
M=D
// push static 0
@Class1.0
D=M
@SP
M=M+1
A=M-1
M=D
// push static 1
@Class1.1
D=M
@SP
M=M+1
A=M-1
M=D
// sub
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M-D
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
// function Sys.init 0
(Sys.init)
@SP
D=M
@LCL
M=D
@SP
M=D
// push constant 6
@6
D=A
@SP
M=M+1
A=M-1
M=D
// push constant 8
@8
D=A
@SP
M=M+1
A=M-1
M=D
// call Class1.set 2
// Save the current stack frame
@Class1.set_RETURN0
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
@7
D=D-A
@ARG
M=D
// Manage the program jumps
@Class1.set
0;JMP
(Class1.set_RETURN0)
// pop temp 0
@SP
M=M-1
A=M
D=M
@5
M=D
// push constant 23
@23
D=A
@SP
M=M+1
A=M-1
M=D
// push constant 15
@15
D=A
@SP
M=M+1
A=M-1
M=D
// call Class2.set 2
// Save the current stack frame
@Class2.set_RETURN1
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
@7
D=D-A
@ARG
M=D
// Manage the program jumps
@Class2.set
0;JMP
(Class2.set_RETURN1)
// pop temp 0
@SP
M=M-1
A=M
D=M
@5
M=D
// call Class1.get 0
// Save the current stack frame
@Class1.get_RETURN2
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
@Class1.get
0;JMP
(Class1.get_RETURN2)
// call Class2.get 0
// Save the current stack frame
@Class2.get_RETURN3
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
@Class2.get
0;JMP
(Class2.get_RETURN3)
// label END
(END)
// goto END
@END
0;JMP
// function Class2.set 0
(Class2.set)
@SP
D=M
@LCL
M=D
@SP
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
// pop static 0
@SP
M=M-1
A=M
D=M
@Class2.0
M=D
// push argument 1
@1
D=A
@ARG
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// pop static 1
@SP
M=M-1
A=M
D=M
@Class2.1
M=D
// push constant 0
@0
D=A
@SP
M=M+1
A=M-1
M=D
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
// function Class2.get 0
(Class2.get)
@SP
D=M
@LCL
M=D
@SP
M=D
// push static 0
@Class2.0
D=M
@SP
M=M+1
A=M-1
M=D
// push static 1
@Class2.1
D=M
@SP
M=M+1
A=M-1
M=D
// sub
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M-D
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