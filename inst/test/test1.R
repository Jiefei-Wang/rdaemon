
setGlobalVariable("a", 2)

getGlobalVariable("a")
existsGlobalVariable("a")


server <- serverSocket(12345)
con <- socketAccept(server, open = "r+")

con <- socketConnection(port = 12345, open = "r+")


readData(con)
readLines(con)


writeData(con, 'test')

writeBin("test", con)

unserialize(readBin(con, "raw", n = 10))
