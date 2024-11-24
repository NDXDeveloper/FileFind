
# INSTALLATION GUIDE - FileFind

This guide explains how to install, compile, and run **FileFind**, an advanced file search utility. The installation steps vary depending on your platform and distribution method.

---

## Prerequisites

Ensure your system meets the following requirements:
- **Operating System:** Windows, Linux, or macOS.
- **Free Pascal Compiler (FPC)** (if building from source).
- **Lazarus IDE** (optional, for source code editing and debugging).

---

## Installation Methods

### Method 1: Installing from Precompiled Binaries

#### **Windows (setup.exe)**
1. Download the latest `FileFind-Setup.exe` from the [Releases](https://github.com/NDXDeveloper/FileFind/releases) page.
2. Run the installer:
   - Double-click `FileFind-Setup.exe`.
   - Follow the installation wizard to install FileFind.
3. Launch FileFind from the Start Menu or the desktop shortcut.

#### **Linux (DEB package)**
1. Download the `.deb` package from the [Releases](https://github.com/NDXDeveloper/FileFind/releases) page.
2. Install the package using your package manager:
   ```bash
   sudo dpkg -i filefind_<version>.deb
   ```
3. If there are missing dependencies, fix them with:
   ```bash
   sudo apt-get install -f
   ```
4. Run FileFind from the applications menu or by typing `filefind` in the terminal.

#### **Linux (RPM package)**
1. Download the `.rpm` package from the [Releases](https://github.com/NDXDeveloper/FileFind/releases) page.
2. Install the package using `dnf` or `yum`:
   ```bash
   sudo dnf install filefind-<version>.rpm
   ```
3. Run FileFind from the applications menu or by typing `filefind` in the terminal.

#### **macOS (DMG file)**
1. Download the `.dmg` file from the [Releases](https://github.com/NDXDeveloper/FileFind/releases) page.
2. Open the `.dmg` file and drag the FileFind icon to the Applications folder.
3. Run FileFind from the Applications folder or Spotlight.

---

### Method 2: Installing from Source

#### Step 1: Clone the Repository
Download the source code from GitHub.

```bash
git clone https://github.com/NDXDeveloper/FileFind.git
cd FileFind
```

#### Step 2: Install Dependencies

- **Windows:**
  1. Install [Free Pascal Compiler (FPC)](https://www.freepascal.org/download.var).
  2. Optionally, install [Lazarus IDE](https://www.lazarus-ide.org/).
  3. Add `fpc` to your system's PATH for command-line compilation (optional).

- **Linux:**
  ```bash
  sudo apt update
  sudo apt install fpc lazarus
  ```

- **macOS:**
  1. Install [FPC](https://www.freepascal.org/download.var) and [Lazarus IDE](https://www.lazarus-ide.org/).
  2. Add FPC and Lazarus binaries to your PATH if necessary.

#### Step 3: Compile the Project

- **Using Lazarus IDE:**
  1. Open `filefind.lpi` in Lazarus.
  2. Go to `Project` → `Options` and ensure the target platform matches your system.
  3. Build the project by pressing `Shift + F9`.
  4. The compiled binary will be in the `bin` directory.

- **Using Command Line:**
  ```bash
  fpc filefind.lpr
  ```
  The binary will be created in the current directory.

#### Step 4: Run the Application
- **Windows:** Double-click the `.exe` file or run it from the terminal:
  ```cmd
  filefind.exe
  ```
- **Linux/macOS:** Run the binary:
  ```bash
  ./filefind
  ```

---

### Method 3: Using a Portable Version

A portable version of **FileFind** is available for Windows and Linux:
- **Windows Portable:**
  1. Download the ZIP archive from the [Releases](https://github.com/NDXDeveloper/FileFind/releases) page.
  2. Extract the ZIP archive to a folder of your choice.
  3. Run `filefind.exe` directly.

- **Linux Portable:**
  1. Download the TAR.GZ archive from the [Releases](https://github.com/NDXDeveloper/FileFind/releases) page.
  2. Extract the archive:
     ```bash
     tar -xvzf filefind-portable-<version>.tar.gz
     ```
  3. Run `./filefind` directly from the extracted folder.

---

## Troubleshooting

### Common Issues
- **Missing Dependencies:** Use your package manager to install missing libraries:
  ```bash
  sudo apt-get install -f
  ```
- **Permission Denied:** Ensure the binary is executable:
  ```bash
  chmod +x filefind
  ```
- **Compilation Errors:** Update FPC/Lazarus to the latest version or ensure all required libraries are installed.

### Getting Help
Open an issue on the [GitHub repository](https://github.com/NDXDeveloper/FileFind/issues) for assistance.

---

## Uninstallation

### From Installer (Windows/Mac/Linux Package Manager)
1. **Windows:**
   - Open the Control Panel → Add/Remove Programs.
   - Find **FileFind** and click "Uninstall."

2. **Linux (DEB/RPM):**
   ```bash
   sudo apt-get remove filefind        # For DEB
   sudo dnf remove filefind            # For RPM
   ```

3. **macOS:**
   - Drag the FileFind app from the Applications folder to the Trash.

---

Thank you for installing FileFind! For additional usage information, check out the [USAGE.md](USAGE.md) file.

