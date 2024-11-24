 

# FileFind - Advanced File Search Utility

FileFind is an advanced utility designed to efficiently locate files and directories based on a variety of criteria such as name, size, attributes, content, and modification dates.

## Features
- **Search by File Name:** Use wildcards (`*`, `?`) to locate files.
- **Recursive Search:** Enable or disable searching in subdirectories.
- **Attribute Filtering:** Include or exclude files based on attributes like:
  - Read-only
  - Hidden
  - System files
  - Directories
- **Content Search:** Find files containing specific phrases with options for:
  - Case sensitivity
  - Whole word matching
  - Negation (files without the phrase)
- **Date and Time Filters:** Locate files by:
  - Creation date/time
  - Modification date/time
  - Last access date/time
- **Size Filters:** Define minimum and maximum file sizes (bytes, KB, MB, GB).
- **Custom Filters:** Add inclusion (`>`) or exclusion (`<`) filters for precise control.
- **Cross-Platform Compatibility:** Works on Windows, Linux, and macOS.
- **Open Files or Locations:** Quickly open files or their containing directories.

## Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/NDXDeveloper/FileFind.git
   cd FileFind
   ```
2. Compile the source code using Free Pascal Compiler (FPC) or Lazarus IDE:
   - Install [Lazarus IDE](https://www.lazarus-ide.org/) and its dependencies.
   - Open the project file `filefind.lpi` in Lazarus IDE.
   - Build the project for your platform (Windows, Linux, macOS).

## Usage
1. Launch the application.
2. **Basic Setup:**
   - Specify the folder to search in the "Folder" field.
   - Use the "File Mask" field to define patterns (e.g., `*.txt`, `file?.doc`).
3. **Optional Filters:**
   - Use the checkboxes to include/exclude attributes like hidden or read-only files.
   - Configure date/time and size filters as needed.
4. **Content Search:**
   - Enter a phrase in the "Content" field.
   - Enable options like case sensitivity or whole word matching.
5. **Run Search:**
   - Click `Start` to begin the search.
   - Use the `Stop` button to cancel an ongoing search.
6. **View Results:**
   - Matching files and folders appear in the list.
   - Double-click an item to open it, or use the right-click menu to open its location.

## Filters
### File Masks
- Use wildcards to search for specific patterns:
  - `*.txt`: All `.txt` files.
  - `file?.doc`: Files like `file1.doc`, `file2.doc`, etc.

### Custom Filters
- Add inclusion or exclusion filters in the "Filters" tab:
  - `>*.log`: Include all `.log` files.
  - `<*.tmp`: Exclude all `.tmp` files.

## Contributing
1. Fork the repository and create a feature branch:
   ```bash
   git checkout -b feature-name
   ```
2. Commit your changes:
   ```bash
   git commit -m "Add feature or fix description"
   ```
3. Push the branch to your fork and submit a pull request.

## License
This project is licensed under the [GNU General Public License v3 (GPLv3)](https://www.gnu.org/licenses/gpl-3.0.html).

## About the Author
- **Developer:** Nicolas DEOUX
- **Email:** [NDXDev@gmail.com](mailto:NDXDev@gmail.com)
- **Website:** [SoftForges.com](https://SoftForges.com)

## Feedback
If you encounter issues or have suggestions, please open an issue on the [GitHub repository](https://github.com/NDXDeveloper/FileFind/issues).

