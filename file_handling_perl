# File handling

Certainly, let's delve into advanced file and folder handling in Perl.

1. **Reading a Directory:**

Perl's `opendir` and `readdir` functions allow you to open a directory and read its contents:

```
opendir(DIR, "/path/to/directory") or die "Couldn't open directory, $!";
while (my $file = readdir(DIR)) {
    print "$file\\n";
}
closedir(DIR);

```

This script prints the names of all items in the directory (including `.` and `..`, which represent the current and parent directories).

1. **Filtering Files by Type or Name:**

You can use the `-f` and `-d` file tests to check if an item is a file or directory, respectively:

```
opendir(DIR, "/path/to/directory") or die "Couldn't open directory, $!";
while (my $file = readdir(DIR)) {
    # Check if the item is a file and its name ends with '.txt'
    if (-f "/path/to/directory/$file" and $file =~ /\\.txt$/) {
        print "$file\\n";
    }
}
closedir(DIR);

```

This script prints the names of all `.txt` files in the directory.

1. **Reading Files in Subdirectories (Recursively):**

You can use the `File::Find` module to traverse a directory tree:

```
use File::Find;

find(\\&wanted, "/path/to/directory");

sub wanted {
    if (-f $_ and $_ =~ /\\.txt$/) {
        print "$File::Find::name\\n";
    }
}

```

This script prints the paths of all `.txt` files in the directory and its subdirectories. `$File::Find::name` is the path to the currently processed file.

1. **Reading and Writing to a File:**

Perl's `open` function lets you open a file for reading (`<`), writing (`>`), or appending (`>>`):

```
# Open a file for reading.
open(INFILE, "<", "/path/to/input.txt") or die "Couldn't open input file, $!";
# Open a file for writing.
open(OUTFILE, ">", "/path/to/output.txt") or die "Couldn't open output file, $!";

while (<INFILE>) {
    print OUTFILE $_;
}

close(INFILE);
close(OUTFILE);

```

This script reads `input.txt` and writes its content to `output.txt`.

1. **Modifying a File In-place:**

Perl's `-i` command-line option allows you to modify a file in-place:

```
perl -pi -e 's/foo/bar/g' /path/to/file.txt

```

This command replaces all occurrences of `foo` with `bar` in `file.txt`.

Remember to always close your files and directories when you're done with them to free up system resources.

These examples illustrate just a few ways Perl can interact with the file system. Perl's built-in functions and CPAN modules provide a wide range of tools for handling even more complex file and directory tasks.

Sure, I'd be happy to dive deeper into advanced file handling in Perl.

1. **Reading a File Backwards:**

The `File::ReadBackwards` module from CPAN can be used to read a file from the end to the start. This is especially useful for large log files where the most recent entries are at the end of the file.

```
use File::ReadBackwards;

my $bw = File::ReadBackwards->new("/path/to/logfile.log") or
    die "Unable to open logfile: $!\\n";
while (defined(my $line = $bw->readline)) {
    print $line;
}

```

1. **Working with CSV Files:**

The `Text::CSV` module from CPAN can be used for parsing and producing CSV files.

Here's an example of parsing a CSV file:

```
use Text::CSV;

my $csv = Text::CSV->new({ binary => 1, auto_diag => 1 });

open my $file, "<:encoding(utf8)", "/path/to/file.csv" or die "Couldn't open file: $!\\n";
while (my $row = $csv->getline($file)) {
    for my $column (@$row) {
        print "$column\\t";
    }
    print "\\n";
}
close $file;

```

1. **Handling JSON Data:**

The `JSON` module can be used to encode and decode JSON data in Perl.

Here's an example of parsing a JSON file:

```
use JSON;

# Read the JSON file.
open my $file, "<:encoding(UTF-8)", "/path/to/file.json" or die "Couldn't open file: $!\\n";
my $json_text = do { local $/; <$file> };

# Decode the JSON data.
my $data = decode_json($json_text);

# Use the data.
for my $item (@{$data->{items}}) {
    print $item->{name}, "\\n";
}

```

1. **File Path Manipulation:**

The `File::Spec` module can be used to handle file paths in a cross-platform way. The `rel2abs` function converts a relative path to an absolute one.

```
use File::Spec;

my $rel_path = "folder/file.txt";
my $abs_path = File::Spec->rel2abs($rel_path);
print "$abs_path\\n";

```

1. **Handling File Archives:**

Perl also has modules to handle file archives. For example, `Archive::Zip` provides functions for creating, modifying, and extracting from Zip archives.

```
use Archive::Zip;

my $zip = Archive::Zip->new();
$zip->addFile("/path/to/file.txt", "file.txt");
unless ($zip->writeToFileNamed("/path/to/archive.zip") == AZ_OK) {
    die "Could not write zip archive.\\n";
}

```

These examples represent more complex use cases for Perl's file handling capabilities. There are many other CPAN modules that can provide additional functionality depending on your specific needs.

Sure, let's explore some other advanced use cases for file handling in Perl using various CPAN modules:

1. **File::Find::Rule - Alternatives to File::Find:**

This module allows you to find files and directories following your own rules. You can perform searches based on name, size, type, and many other attributes:

```
use File::Find::Rule;

# Find all .txt files in /path/to/directory and its subdirectories
my @files = File::Find::Rule->file()
                            ->name('*.txt')
                            ->in('/path/to/directory');

```

1. **Path::Tiny - A Minimalist File Handling Module:**

Path::Tiny module provides a lightweight, intuitive, and flexible way to handle file paths and files:

```
use Path::Tiny;

# Read a file
my $content = path("/path/to/file.txt")->slurp_utf8;

# Write to a file
path("/path/to/file.txt")->spew_utf8("New content");

# Iterate over lines in a file
for my $line (path("/path/to/file.txt")->lines_utf8({chomp => 1})) {
    print "$line\\n";
}

# Get the parent directory
my $parent_dir = path("/path/to/file.txt")->parent;

```

1. **File::Copy::Recursive - Recursively Copy Files and Directories:**

This module provides functions for recursively copying files and directories:

```
use File::Copy::Recursive qw(dircopy);

dircopy("/source/directory", "/destination/directory") or die $!;

```

1. **File::Slurp - Read/Write Entire Files with a Single Command:**

This module provides functions for reading or writing entire files with a single command:

```
use File::Slurp;

# Read an entire file into a scalar
my $text = read_file('/path/to/file.txt');

# Write a scalar to an entire file
write_file('/path/to/file.txt', $text);

```

1. **File::Temp - Dealing with Temporary Files:**

This module helps in creating temporary files and directories. It provides a variety of ways of maintaining and controlling the lifecycle of the files:

```
use File::Temp qw(tempfile);

my ($fh, $filename) = tempfile();
print $fh "Some text...\\n";

```

Each of these examples demonstrates Perl's extensive capabilities for handling files and directories using advanced features and modules. Perl's flexibility and robustness make it suitable for a wide range of tasks, from basic file manipulation to complex I/O operations.

Sure, let's look at some more advanced and complex I/O operations you might perform in Perl.

**1. Process IO:**

Perl's `open` function can be used to open a pipe to a command, allowing you to interact with the command's input and output.

Here's an example of piping output to a system command:

```
open(my $fh, "|-", "sort", "-n") or die "Couldn't open pipe to sort: $!";
print $fh $_ for 5, 2, 3, 4, 1;  # Prints numbers in sorted order
close($fh);

```

And here's an example of capturing output from a system command:

```
open(my $fh, "-|", "ls", "-l") or die "Couldn't open pipe from ls: $!";
while (<$fh>) {
    print;  # Prints output from `ls -l`
}
close($fh);

```

**2. IO::All:**

The `IO::All` module provides a simple and flexible interface for I/O operations in Perl. It supports file, directory, socket, and process I/O, among other things.

Here's an example of reading and writing files with `IO::All`:

```
use IO::All;

my $contents = io("/path/to/file.txt")->all;  # Read a file

io("/path/to/file.txt")->print("Hello, world!");  # Write to a file

```

Here's an example of listing directory contents:

```
use IO::All;

my @files = io("/path/to/directory")->all;  # Get directory contents

for my $file (@files) {
    print $file->name, "\\n";  # Print each file/directory name
}

```

And here's an example of capturing command output:

```
use IO::All;

my $output = io("ls -l |")->all;  # Capture output from `ls -l`
print $output;

```

**3. Reading and Writing Binary Data:**

Perl can handle binary data as well as text. Here's an example of reading binary data from a file:

```
open(my $fh, "<:raw", "/path/to/file.bin") or die "Couldn't open binary file: $!";
binmode(STDOUT);  # Needed to print binary data correctly

while (read($fh, my $buffer, 1024)) {
    print $buffer;  # Prints binary data
}

close($fh);

```

These examples demonstrate a range of advanced I/O operations you might perform in Perl. As always, the Perl documentation and CPAN are excellent resources if you need to do something not covered here.

Perl has a vast ecosystem of modules, thanks to CPAN. For advanced file handling, there are several modules available that can handle a multitude of complex tasks. Here are a few that stand out:

1. **MooseX::FileAttribute:**

This module integrates file handling into Moose objects. This means you can create an object that has certain file-related attributes (like a path), and methods for interacting with that file.

```
use Moose;
use MooseX::FileAttribute;

has_file 'data' => (
    traits => ['TextFile'],
    isa => 'Str',
    is => 'rw',
    required => 1,
);

# The object now has methods for opening, reading, and writing to the file.
my $object = Class->new( data => "/path/to/data.txt" );
$object->data_append("Some new data.\\n");

```

1. **Path::Class:**

Path::Class is a cross-platform module for manipulating file and directory specifications in a convenient object-oriented way. It has classes representing files and directories and provides methods for working with them.

```
use Path::Class;

my $dir = dir('foo', 'bar');       # Path::Class::Dir object
my $file = $dir->file('baz.txt');  # Path::Class::File object

# Print out the directory
print $dir->stringify;

# Open the file for reading
my $file_handle = $file->openr();

```

1. **IO::Prompter:**

Although not strictly a file-handling module, IO::Prompter can make your scripts more interactive by prompting users for input. This could be useful for scripts that require user input to determine which files to process.

```
use IO::Prompter;

my $filename = prompt 'Enter a filename:', -echo;

# Now, you can open $filename for reading or writing.

```

1. **File::Map:**

This module memory-maps files to make file reading and writing more efficient. It's especially useful for large files.

```
use File::Map 'map_file';

map_file my $map, "/path/to/large_file.txt";

```

1. **Tie::File:**

Tie::File represents a regular text file as a Perl array. Each element in the array corresponds to a record in the file.

```
use Tie::File;

tie my @array, 'Tie::File', "/path/to/file.txt" or die $!;

# Print the first line in the file
print $array[0];

```

Remember, these are just examples. The choice of the CPAN module depends highly on your specific requirements, the complexity of the task at hand, and your personal preferences
