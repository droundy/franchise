import Distribution.Franchise

configure = do findPackagesFor "Main.hs"

main = build [] configure $ executable "hello" "Main.hs" []
