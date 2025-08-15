import os

def main():
    count = int(os.getenv("PCOUNT", "200000"))
    i = 0
    while i < count:
        print("hello")
        print("world")
        print("aether")
        i += 1

if __name__ == "__main__":
    main()
