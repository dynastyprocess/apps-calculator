FROM rhub/r-minimal:4.2.0

RUN apk add --no-cache --update-cache \
        --repository http://nl.alpinelinux.org/alpine/v3.11/main \
        autoconf=2.69-r2 \
        automake=1.16.1-r0

COPY DESCRIPTION /root

RUN installr -d \
    # repeat autoconf and automake (under `-t`)
    # to (auto)remove them after installation
        -t "libsodium-dev curl-dev gfortran linux-headers autoconf automake libxml2-dev" \
        -a "libsodium libxml2 curl"\
        local::.

COPY . /root/app

CMD ["R", "-e", "shiny::runApp('/root/app',host='0.0.0.0',port=3838)"]
