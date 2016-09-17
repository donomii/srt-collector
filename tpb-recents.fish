for x in ( wget -O- https://thepiratebay.org/recent | perl -ne'while(/(magnet.+?)"/sg){print $1."\n";}')
    go run main.go --dir='./' --pick=asdf $x &
end ;
