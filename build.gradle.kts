plugins {
    kotlin("jvm") version "1.9.22"
    id("org.jetbrains.dokka") version "1.5.30"
}

group = "org.fancryer.bf"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation("org.jetbrains.kotlin:kotlin-test")
    implementation("io.arrow-kt:arrow-core:1.2.1")
    implementation("io.arrow-kt:arrow-fx-coroutines:1.2.1")
    api("org.antlr:antlr4:4.13.0")
    api("org.antlr:antlr4-runtime:4.13.0")
    implementation(kotlin("reflect"))
    implementation("org.jetbrains.dokka:mathjax-plugin:1.5.31")
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}