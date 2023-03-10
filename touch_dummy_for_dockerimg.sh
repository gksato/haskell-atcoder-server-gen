#! /bin/sh

datefmt_iso8601() {
    if date -j >/dev/null 2>&1; then
        date -u -j -Iseconds -f "%Y-%m-%d %H:%M:%S %z" "$1"
    else
        date -u -Iseconds -d "$1"
    fi
}

imgdate=$(docker image ls --format "{{.CreatedAt}}" "$2" | \
              sed -e 's/ [A-Z][A-Z][A-Z]$//')
if [ -n "$imgdate" ]; then
    imgdate8601=$(datefmt_iso8601 "$imgdate" | \
               sed -e 's/+00:\{0,1\}00$/Z/')
    mkdir -p $(dirname $1)
    touch -d $imgdate8601 $1
else
    rm -f $1
fi



