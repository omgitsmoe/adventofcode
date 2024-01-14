package main

import (
    "fmt"
    "os"
    "strings"
    "container/list"
    "bytes"
    "strconv"
)

func main() {
    data, err := os.ReadFile("d15_input.txt")
    if err != nil {
        panic(err)
    }
    input := strings.TrimSpace(string(data))

    steps := strings.Split(input, ",")
    part1 := 0
    for _, step := range steps {
        hash := hash([]byte(step))
        fmt.Println(step, "->", hash)
        part1 += hash
    }
    fmt.Println("Part1:", part1)

    // use a List to store the lenses in the box, in case there are a lot
    // of inserations/moves
    boxes := make([]list.List, 256)
    for _, step := range steps {
        doStep(boxes, []byte(step))
    }
    part2 := 0
    for boxIdx := range boxes {
        slotNumber := 1
        for e := boxes[boxIdx].Front(); e != nil; e = e.Next() {
            // var foo any
            // foo = Lens{..}
            // foo.(Lens).label
            //     ^ type assertion
            lens := e.Value.(*Lens)
            focusingPower := (boxIdx + 1) * slotNumber * lens.focalLength
            part2 += focusingPower
            fmt.Printf("%s: %v * %v * %v\n",
                lens.label, boxIdx + 1, slotNumber, lens.focalLength)
            slotNumber += 1
        }
    }
    fmt.Println("Part2:", part2)
}

func hash(value []byte) int {
    hash := 0
    for _, r := range value {
        ascii := int(r)
        hash += ascii
        hash *= 17
        // hash %= 256
        const moduloMask = 256 - 1
        hash &= moduloMask
    }

    return hash
}

type Lens struct {
    label []byte
    focalLength int
}

func doStep(boxes []list.List, step []byte) {
    operationIdx := len(step) - 1
    for offset := range step {
        // mark this used otherwise go doesn't compile
        _ = offset
        if step[operationIdx] == byte('-') || step[operationIdx] == byte('=') {
            break
        }
        operationIdx -= 1
    }
    label := step[:operationIdx]
    operation := step[operationIdx]
    boxIdx := hash(label)
    if operation == byte('-') {
        // remove lens with label
        boxContents := &boxes[boxIdx]
        for e := boxContents.Front(); e != nil; e = e.Next() {
            // type assertion, casting any to the proper type
            // CAREFUL!!!!! this makes a copy
            // lens := e.Value.(Lens)
            // and you can't take the address of a type assertion
            // lens := &e.Value.(Lens)
            // also doesn't work, since it also makes a copy:
            // e.Value.(Lens).label = label
            // => need to store a ptr
            lens := e.Value.(*Lens)
            if bytes.Equal(label, lens.label) {
                boxContents.Remove(e)
                break
            }
        }
    } else if operation == byte('=') {
        focalLength, err := strconv.Atoi(string(step[operationIdx + 1:]))
        if err != nil {
            // panic(fmt.Sprintf("failed to convert", string(step[operationIdx + 1:])))
            panic(err)
        }
        boxContents := &boxes[boxIdx]
        replaced := false
        for e := boxContents.Front(); e != nil; e = e.Next() {
            lens := e.Value.(*Lens)
            if bytes.Equal(label, lens.label) {
                // replace old with new lens
                lens.label = label
                lens.focalLength = focalLength
                replaced = true
                break
            }
        }

        if !replaced {
            // add lens to the back
            // NOTE: need to store a pointer, since in Go you can't take the
            //       address of sth. that stored in an interface
            //       (here any/interface{})
            newLens := Lens{label: label, focalLength: focalLength}
            boxContents.PushBack(&newLens)
        }
    } else {
        panic(fmt.Sprintf("unknown operation %c", operation))
    }
}
