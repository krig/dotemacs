#include <zlib.h>
#include <stdio.h>

int main(int argc, char** argv) {
    gzFile fil;
    char linebuf[1024] = "!";
    fil = gzopen(argv[1], "r");
    while (linebuf[0] != 0) {
        gzgets(fil, linebuf, 1024);
        if (linebuf[0]) {
            printf("%s", linebuf);
        }
        }
    gzclose(fil);
    return 0;
}

