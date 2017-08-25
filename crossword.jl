using Images

type SimpleColor
    r
    g
    b
end

type CrosswordAnswer
    word::String
    word_length::Int64

    choices

    positions

    crosses
end

type CrosswordPuzzle
    across
    down
end

+(f::Function, g::Function) = x -> f(g(x))

function ==(a::SimpleColor, b::SimpleColor)
    return a.r == b.r && a.b == b.b && a.g == b.g
end

#To be expanded. Add support for other varieties of color storages.
function simplify_color(other)
    try #ARGH. Silly work-around because I don't know how this works.
        return SimpleColor(float32(other.c.r), float32(other.c.g), float32(other.c.b))
    catch
        try
            return SimpleColor(float32(other.r), float32(other.g), float32(other.b))
        catch
            return SimpleColor(float32(other), float32(other), float32(other))
        end
    end
end

function brightness(color)
    return (color.r + color.g + color.b) / 3
end

function get_all_matches(len::Int64, tests, words)
    result = String[]
    fails = Int64[]

    for i in words
        success = true

        if length(i) != len
            continue
        end

        for (test_index, (index, char)) = enumerate(tests)
            if i[index] != char
                success = false

                push!(fails, test_index)

                break
            end
        end

        if success
            push!(result, i)
        end
    end

    return (result, fails)
end

function create_tests(a::CrosswordAnswer)
    result = (Int64, Char)[]

    for (word, position, word_position) = a.crosses
        if length(word.word) > 0
            push!(result, (position, word.word[word_position]))
        end
    end

    return result
end

function clear_crossword(a::CrosswordPuzzle)
    for i = a.across
        i.word = ""
    end

    for i = a.down
        i.word = ""
    end

    return a
end

function get_all_words()
    f = open("words.txt")

    words = String[i[1:end-2] for i = readlines(f)]

    close(f)

    return words
end

function get_all_choices(a, words)
    for i = a
        if length(i.word) == 0
            i.choices, fails = get_all_matches(i.word_length, create_tests(i), words)
            fails = filter(k -> i.crosses[k][1].word != "", unique(fails))

            if length(i.choices) == 0
                return (a, map(k -> (i.crosses[k][1].word, (i.crosses[k][1].word[i.crosses[k][3]], i.crosses[k][3])), fails))
            end
        else
            i.choices = [i.word]
        end
    end

    return (a, [])
end

function is_valid(a)
    for i = a
        if length(i.choices) == 0
            return false
        end
    end

    return true
end

product(a::Array{Int64, 1}) = reduce(*, map(i -> BigInt(i), a))

function try_combinations(a, words, tab_level, is_outer = false)
    if !is_valid(a)
        throw("Invalid combination!")
    end

    if length(a) == 1
        result = CrosswordAnswer[]
    else
        result = a[2:end]
    end

    skipped = 0

    fail = (Char, Int64)[]

    #println("$(repeat(" ", tab_level))$(product([length(i.choices) for i = result])) possible choices.")

    for (index, choice) = enumerate(a[1].choices)
        if length(fail) > 0
            cont = false

            i = 1
            while i < length(fail)
                char, ind = fail[i]

                if choice[ind] == char
                    cont = true

                    break
                else
                    deleteat!(fail, i)
                end
            end

            if cont
                skipped += 1

                continue
            end
        end

        a[1].word = choice

        test, fails = get_all_choices(result, words)

        message = rpad(repeat(" ", tab_level) * "$(round(index / length(a[1].choices) * 100, 4))% done, $index out of $(length(a[1].choices)) = $choice, fails = $fails, length(fail) = $(length(fail))", 193, " ")

        print("\r$message")

        if length(fails) > 0
            for (word, (char, ind)) = fails
                if word == choice
                    push!(fail, (char, ind))
                end
            end
        end

        try
            if is_valid(test)
                message = "  $choice is valid so far, $skipped skipped  "
                lpad_len = 97 + div(length(message), 2)
                rpad_len = 192

                message = rpad(lpad(message, lpad_len, "-"), rpad_len, "-")

                println("\n$message")

                result = try_combinations(test, words, tab_level + 1)

                break
            else
                if length(fails) == 1 && length(a[1].choice) == 1
                    throw(fails[1])
                elseif index == length(a[1].choices) #We dun failed
                    if length(fails) == 1
                        throw(fails[1])
                    else
                        throw("Invalid combination!")
                    end
                end
            end
        catch e
            if e == "Invalid combination!"
                a[1].word = ""

                if index == length(a[1].choices) #We dun failed
                    throw("Invalid combination!")
                end
            elseif typeof(e) == (String, (Char, Int64))
                if e[1] == a[1].word
                    a[1].word = ""

                    if !(e[2] in fail)
                        push!(fail, e[2])
                    end
                else
                    a[1].word = ""

                    throw(e)
                end
            end
        end
    end

    return vcat(a, result)
end

function solve_crossword(a::CrosswordPuzzle)
    words = get_all_words()
    result, fails = get_all_choices(vcat(a.across, a.down), words)

    println("$(length(result)), $(length(vcat(a.across, a.down)))")
    println("$(product([length(i.choices) for i = result])) possible choices.")

    result = try_combinations(result, words, 0, true)

    for (i,k) = enumerate(result)
        if i <= length(a.across)
            a.across[i] = k
        elseif i <= length(a.across) + length(a.down)
            a.down[i - length(a.across)] = k
        end
    end

    return a
end

function show_puzzle(a::CrosswordPuzzle, size)
    grid = fill('\#', size)

    for i = a.across
        for (index, char) = enumerate(i.word)
            x,y = i.positions[index]

            grid[y,x] = char
        end
    end

    for i = a.down
        for (index, char) = enumerate(i.word)
            x,y = i.positions[index]

            grid[y,x] = char
        end
    end

    show_puzzle_grid(grid)

    println("Across:")

    for i = a.across
        println("$((i.word, i.word_length)); Crosses: $([(cross.word, (i.word[j], j), (cross.word[k], k)) for (cross, j, k) = i.crosses])")
    end

    println("\nDown:")
    for i = a.down
        println("$((i.word, i.word_length)); Crosses: $([(cross.word, (i.word[j], j), (cross.word[k], k)) for (cross, j, k) = i.crosses])")
    end
end

function show_puzzle_grid(grid)
    for row = 1:size(grid)[1]
        for column = 1:size(grid)[1]
            print(grid[row, column])
            print(' ')
        end

        println("")
    end
end

function read_crossword_puzzle(fname)
    f = open(fname)

    lines = readlines(f)
    size = (length(lines[1]) - 1, length(lines))

    grid = fill(0, size)

    for i = 1:size[2]
        vals = [Int(i) - 48 for i = lines[i][1:end-1]]
        grid[1:size[1], i] = vals
    end

    close(f)

    print(grid)

    return make_crossword_input(grid)
end

function manual_input_crossword_puzzle()
    println("Input size of crossword puzzle: w h")
    size = map(int, split(readline()))
    size = (size[1], size[2])

    grid = fill(0, size)

    println("Enter crossword puzzle line by line. 1 is an empty square (white), 0 is a closed square (black). Do not put spaces in between squares.")
    for i = 1:size[2]
        vals = [Int(i) - 48 for i = readline()[1:end-1]]

        println(vals)

        grid[1:size[1], i] = vals
    end

    return make_crossword_input(grid)
end

function make_crossword_input(grid)
    across, down = create_answers(grid)

    a = CrosswordPuzzle(across, down)
    a = solve_crossword(a)

    show_puzzle(a, size)

    return a
end

function make_random_crossword(w, h, fname)
    grid = fill(0, (w, h))

    for x in 1:h
        for y in 1:w
            grid[x,y] = rand([1,0])
        end
    end

    f = open(fname, "w")

    for x in 1:h
        write(f, "$(join(grid[x,1:w], ""))\n")
    end

    close(f)

    return fname
end

function crossword_puzzle(img::String, squares)
    read_im = imread(img)

    squares_on_top =

    grid = make_grid(read_im, squares)
    show_grid(grid)

    across, down = create_answers(grid)
    a = CrosswordPuzzle(across, down)
    a = solve_crossword(a)

    show_puzzle(a, (squares, squares))

    return a
end

function create_answers(a)
    across_list = CrosswordAnswer[]
    down_list = CrosswordAnswer[]

    for x = 1:size(a)[1]
        for y = 1:size(a)[2]
            if a[x,y] == 1
                left = false
                up = false
                right = false
                down = false

                if x > 1
                    left = a[x - 1,y] == 1
                end
                if x < size(a)[1]
                    right = a[x + 1,y] == 1
                end
                if y > 1
                    up = a[x, y - 1] == 1
                end
                if y < size(a)[1]
                    down = a[x, y + 1] == 1
                end

                if right && !left
                    line = (Int64, Int64)[]

                    push!(line, (x, y))

                    for i=(x + 1):size(a)[1]
                        if a[i, y] == 1
                             push!(line, (i, y))
                        else
                            break
                        end
                    end

                    push!(across_list, CrosswordAnswer("", length(line), String[], line, []))
                end

                if down && !up
                    line = (Int64, Int64)[]

                    push!(line, (x, y))

                    for i=(y + 1):size(a)[2]
                        if a[x, i] == 1
                             push!(line, (x, i))
                        else
                            break
                        end
                    end

                    push!(down_list, CrosswordAnswer("", length(line), String[], line, []))
                end
            end

            print("\r$(round((x * (size(a)[1] - 1) + y ) / length(a) * 100, 2))% done.")
        end
    end

    println("")
    println("Creating crosses for the across answers.")
    println("")

    for (i,k) = enumerate(across_list)
        crosses = (CrosswordAnswer, Int64, Int64)[]

        for (index, pos) = enumerate(k.positions)
            for answers = down_list
                answer_index = findfirst(answers.positions, pos)

                if answer_index > 0
                    push!(crosses, (answers, index, answer_index))
                end
            end
        end

        k.crosses = crosses

        print("\r$i of $(length(across_list)) done.")
    end

    println("")
    println("Creating crosses for the down answers.")
    println("")

    for (i,k) = enumerate(down_list)
        crosses = (CrosswordAnswer, Int64, Int64)[]

        for (index, pos) = enumerate(k.positions)
            for answers = across_list
                answer_index = findfirst(answers.positions, pos)

                if answer_index > 0
                    push!(crosses, (answers, index, answer_index))
                end
            end
        end

        k.crosses = crosses

        print("\r$i of $(length(down_list)) done.")
    end

    println("")

    return (across_list, down_list)
end

function make_grid(im, squares)
    square_size = div(width(im), squares)

    result = fill(0, (squares, squares))

    println("square_size = $square_size")

    for y = 1:squares
        for x = 1:squares
            #It's white
            pixel_x = x * square_size - (div(square_size, 2) + 1)
            pixel_y = y * square_size - (div(square_size, 2) + 1)

            if brightness(simplify_color(im[pixel_x, pixel_y])) > 0.5
                result[x,y] = 1
            end

            print("\r$(round((y * (squares - 1) + x ) / (squares ^ 2) * 100, 2))% done.")
        end
    end

    println("")

    return result
end

function show_grid(grid)
    for row = 1:size(grid)[1]
        for column = 1:size(grid)[1]
            if grid[column, row] == 1
                print("[]")
            elseif grid[column, row] == 2
                print("##")
            else
                print("--")
            end
        end

        println("")
    end
end

#defined for convenience
black = SimpleColor(float(0.0), float(0.0), float(0.0))
white = SimpleColor(float(1.0), float(1.0), float(1.0))
