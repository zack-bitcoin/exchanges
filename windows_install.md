Whilst the official rebar documentation only covers usages on Unix, turns out it was really easy to get going on Windows too, even without resorting to installing and using cygwin.

Here’s some quick steps:

    clone the rebar repo

    in the root of the repo, run bootstrap.bat (you’ll need Erlang installed for this)

    copy the generated rebar and rebar.cmd files to a folder on your PATH, I typically have a C:\tools folder for miscellaneous tools like this


From here on, you can resume the rebar Getting Started guide to create your first rebar project.