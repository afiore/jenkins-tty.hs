# Jenkins TTY

[![Build Status](https://travis-ci.org/afiore/jenkins-tty.hs.svg)](https://travis-ci.org/afiore/jenkins-tty.hs)

A minimalistic command line interface to the popular continuous integration tool Jenkins.

## Rationale

Jenkins comes packed with features, but it lacks a simple, command line
interface suitable for day to day use. While the built-in [Jenkins CLI](https://wiki.jenkins-ci.org/display/JENKINS/Jenkins+CLI) 
tool allows to peerform administrative operations (e.g. loading jobs and plugins)
as well as key tasks such as triggering builds, it does not provide a
convenient way to inspect job statuses, or list job builds. Additionally,
as most programs running on the JVM, Jenkins CLI suffers from a rather long startup time,
which is arguably undesirable for a command line tool.

## Usage

    jenkins-tty -s JENKINS_URL [-u HTTP_AUTH] COMMAND

Currently implemented commands are:

- `jobs`: Lists jobs with their respective status.
- `job`: Lists the builds associated to a specific job.
- `build`: Triggers a build optionally allowing to supply custom build parameters (eg. `GIT_REV`).
- `log`: Prints a build log

A command-specific synopsis can be obtained by running a command with no arguments.

## Building

Jenkins-tty can be built using Haskell's packaging system [cabal](https://www.haskell.org/cabal/).
From the project directory, run:

    cabal init sandbox && cabal install
    
You should now find the jenkins-tty executable in the `.cabal-sandbox/bin` folder.
Alternatively, pre-compiled binaries are [also available](https://github.com/afiore/jenkins-tty.hs/releases).
