package main

import (
	// "errors"
	"bufio"
	"fmt"
	"os"
)

func main() {
	err := run()
	if err != nil {
		panic(err)
	}
	//
	// fmt.Println("hello")
	// fmt.Println(os.Args)
	//
	// if len(os.Args) != 2 {
	// 	panic("Expected CLI 1 argument, got ")
	// }
}

func run() error {
	// The first arg is the program name; discard it
	args := os.Args[1:]
	if len(args) != 1 {
		return fmt.Errorf("Expected 1 CLI argument, got the following args: %v", args)
	}
	return nil
}

func doFile(filePath string) error {
	file, err := os.Open(filePath)
	if err != nil {
		return err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	return nil
}
