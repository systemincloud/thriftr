enum Lang {
    C,
    Go,
    Java,
    Javascript,
    PHP,
    Python,
    Ruby,
}


enum Country {
    US = 1,
    UK,
    CN,
}


enum OS {
    OSX,
    Win = 3,
    Linux
}

struct Conf {
  1: Lang lang=Lang.PHP
  2: OS os=OS.Linux
}
