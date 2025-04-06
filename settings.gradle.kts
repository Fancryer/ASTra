plugins {
	id("org.gradle.toolchains.foojay-resolver-convention") version "0.5.0"
}
rootProject.name = "KAST"
include("src:main:ast")
findProject(":src:main:ast")?.name = "ast"
include("src:ast")
findProject(":src:ast")?.name = "ast"
