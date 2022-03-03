function today
    set filename (date -I).md
    echo "$filename"
    touch "$filename"
    commandline vi\ "$filename"
end
