package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
)

func main() {
    b, err := os.Open("d5_input.txt")
    if err != nil {
        fmt.Println(err)
    }
    scanner := bufio.NewScanner(b)
    // Scan advances the Scanner to the next token, which will then be
    // available through the Bytes or Text method
    scanner.Scan()
    // slice of byte
    inp := []byte(strings.TrimSpace(scanner.Text()))
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }
    // go passes almost? everything by value so an assignment is like a copy operation
    // but it doesnt work for slices (would work for arrays or structs) and inp is a slice of bytes
    // inp_copy = inp
    // The copy function supports copying between slices of different lengths (!!! it will copy only up to the smaller number of elements !!!)
    // so make them the same size by passing len(inp)
    inp_copy := make([]byte, len(inp))
    // dest, src
    copy(inp_copy, inp)
    i := 0
    for {
        if i > len(inp_copy)-2 {
            break
        } else if int(inp_copy[i]) - int(inp_copy[i+1]) == 32 || int(inp_copy[i]) - int(inp_copy[i+1]) == -32 {
            // del i and i+1 from array by appending all elements beginning with i+2
            // to slice ending before i
            inp_copy = append(inp_copy[:i], inp_copy[i+2:]...)
            if i > 0 {
                i--
            }
        } else {
            i++
        }
    }
    fmt.Println("Part1:", len(inp_copy))

    var lengths [26][2]int
    for j := range lengths {
        c := 97 + j
        inp_str := strings.Replace(strings.Replace(string(inp), string(c), "", -1), string(c-32), "", -1)
        inp_copy = []byte(inp_str)
        i := 0
        for {
            if i > len(inp_copy)-2 {
                break
            } else if (int(inp_copy[i]) - int(inp_copy[i+1]) == 32) || (int(inp_copy[i]) - int(inp_copy[i+1]) == -32) {
                // fmt.Println(string(inp_copy[i-1]), string(inp_copy[i]), string(inp_copy[i+1]), string(inp_copy[i+2]))
                // del i and i+1 from array by appending all elements beginning with i+2
                // to slice ending before i; ... unpacks slice into var args
                inp_copy = append(inp_copy[:i], inp_copy[i+2:]...)
                if i > 0 {
                    i--
                }
            } else {
                i++
            }
        }        
        lengths[j][0] = c
        lengths[j][1] = len(inp_copy)
    }
    min := -1
    for _, c_len := range lengths {
        if c_len[1] < min {
            min = c_len[1]
        } else if min == -1 {
            min = c_len[1]
        }
    }
    fmt.Println("Part2:", min)
    }
