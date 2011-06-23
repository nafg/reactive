resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

resolvers += Resolver.url("Typesafe", url("http://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)

libraryDependencies <+= sbtVersion{v => "com.github.siasia" %% "xsbt-web-plugin" % (v)}
