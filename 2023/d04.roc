app "d04"
    # packages used by this application
    # import the basic-cli platform package as `pf`
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    # import modules? from the pf package
    imports [pf.Stdout, pf.File,
             pf.Path, pf.Task, # pf.Task.{ await } import await directly
            ]
    provides [main] to pf


filename = "d04_example.txt"

main =
    # using callbacks for tasks explicitly can become unreadable
    # pretty quickly. it's better to use the backpassing operator
    # `<-`, which creates an anonymous function, binds it to the
    # left and passes it to whatever comes after
    input <- (File.readUtf8 (Path.fromStr filename))
             |> Task.onErr \_ -> Task.ok "oops"
             |> Task.await
    parse_line = \line ->
        line
        |> Str.split ":"
        |> List.map (\s ->
            s
            |> Str.trim
            |> Str.split "|"
            |> List.map (\s -> s |> Str.trim |> Str.split " "))
    # has to end with a task to return it
    Stdout.line (input |> parse_line |> List.get 0 |> Inspect.toStr)
    # Task -> effect outside Roc, implemented by platform
    # e.g. File.readUtf8 returns
    # Task Str [FileReadErr Path ReadErr, FileReadUtf8Err Path]
    #      ^ return  ^ possible errors
    #      errors are Tags (everything that's capitalized is a tag/type?)
    #      they can have payloads -> tagged union
    # Task.await to combine tasks into a bigger one
    # (takes a callback as 2nd param which takes Task a's
    #  output and returns Task b)
    # (File.readUtf8 (Path.fromStr filename))
    # # handle possible errors: this needs to return a task
    # # as well, where the original task is Task a b and the new
    # # task is Task a c, so the return value type needs to stay the
    # # same;
    # # `Task.ok a` returns a task that always returns a
    # # alternative would be using Task.attempt
    # |> Task.onErr \_ -> Task.ok "oops"
    # |> Task.await \input ->
    #     Stdout.line input


    # NOTE: tried to handle the error manually, but I couldn't get
    # it to work; from what I've seen it's actually not possible
    # and you need to use one of the two functions: onErr / attempt
    # Task.await (File.readUtf8 (Path.fromStr filename)) \input ->
    #     # function with paramter `input`                ^
    #     # pipe |> has to be at exact same indentation level.. wtf
    #     # (passes arg before to first arg of the following func)
    #     # Stdout.line "ada"
    #     # |> Task.await
    #     when input is
    #     # variable shadowing e.g. overwriting input here does not work
    #     Str -> Task.await (Stdout.line inp)
    #     # string interpolation using \(expr) inside ""
    #     FileReadErr path err -> Task.ok "fsdds"
    #     FileReadUtf8Err path -> Task.ok "dasa"


    
