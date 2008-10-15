import Distribution.Franchise

configure = findPackagesFor "Main.hs"
main = build [] configure $ executable "tester" "Main.hs" ["test.c"]
