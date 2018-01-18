# Tripitaka


This project is depreceted. If you are interested in a static site generator written by Common Lisp, please check new project [atsuage](https://github.com/myaosato/atsuage)







Tripitaka is static website management library wrtien by Common Lisp.

This product is still a development version.

__Maybe many bugs are still included. Sorry... X(__

## Overview

Tripitaka provide the following three

* to make HTML file
    * generate HTML file from a text file like plist of Common Lisp and a template like HTML file
    * help to make article page for diary
    * ...
* to generate simple Atom feed
* simple ftp-client (However this still dose not support FTP over SSL.

## How to use
__This documentation is not yet complete.__

### Prepare directory and files

Please call function tripitaka:ready.

    CL-USER> (tripitala:ready)

#### ~/.tripitakarc

If .tripitakarc file dosen't exist at home directory when (tripitaka:ready) is called, ~/tripitaka/sample/ and ~/.tripitakarc are created.

If you don't like this situation, please make ~/.tripitakarc file like as the following.

    :project-name "/your/project/directry/path/"

For example

    :sample "~/tripitaka/sample/"

When (tripitaka:ready) is called, a project at first line in .tripitakarc is selected. If you want to chage project, you can use (triptaka:set-project "project-name").

#### project

Please make text file named _project_ under your-project directory like as the following

    :site-name "SAMPLE WEBSITE"
    :site-url "sample://eaxample.com"
    :author "Your name"
    :pubyear "2017"

Tripitaka make html file by using these informations.

#### .dat files

We make data files for Tripitaka in dat/ directroy under a project directry.

These extensions are .dat. For example "index.dat". A content example is the following

    :title "TITLE OF PAGE"
    :date "YYYY-MM-DD"
    :up "upper page"
    :prev "previous page"
    :next "next page"
    :text
    This is a sample text.

    Hello Tripitaka!

    You can use markdown notation.

    * sample list
    * foo
    * bar

These file include symbol (:title, :date, ...) and value (string) pairs and main text of your page.

Tripitaka uses this information to make HTML files. (index.dat -&gt; index.htm)

#### template

Please put template file (named template) on theme/ directry under your project directry.

template files are like HTML. However Tripitaka reads the following placeholders

    <tri:project prop="foo">
    <tri:page prop="bar">
    <tri:page prop="baz" name="qux">

##### &lt;tri:project prop="foo"&gt;

This placeholder means a property of project file.

For example, in the case, project file

    :site-name "SAMPLE WEBSITE"
    :site-url "sample://eaxample.com"
    :author "Your name"
    :pubyear "2017"

and  a description exists in templatefile

    <h1><tri:project prop="site-name"></h1>

Tripitaka replaces this description with

    <h1>SAMPLE WEBSITE</h1>

##### &lt;tri:page prop="bar"&gt; and &lt;tri:page prop="baz" name="qux"&gt;

In this case, Tripitaka replaces placeholders with value of .dat file that Tripitakba creates html file from.

For example, next discripiton exist in index.dat.

    :title "THIS IS HOME PAGE"

and

    :title "ABOUT THIS WEB SITE"

in readme.dat.

Tripitaka replaces

    <tri:page prop="title">

with

    THIS IS HOME PAGE

If you want to refer other file when make index.htm (it is make from index.dat), you can use name attribute in placeholders.

Tripitaka convert

    please check <a href="readme.htm"><tri:page prop="title" name="readme"></a>

into

    please check <a href="readme.htm">ABOUT THIS WEB SITE</a>

And...


## License

This software is released under the MIT License, please check LICENSE.txt.
