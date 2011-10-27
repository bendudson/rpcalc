
TARGET=rpcalc

$(TARGET): $(TARGET).hs
	ghc -o $(TARGET) $(TARGET).hs
