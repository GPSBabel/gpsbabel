<#
.SYNOPSIS

Run GPSBabel GUI or CLI.

.DESCRIPTION

A docker container containing the GPSBabel GUI and CLI is created and the
GUI or CLI is run. It is assumed you are running on Windows, Docker Desktop
is installed and has been started, and WSL 2 is installed. Your Desktop is
available in the container /app directory. This container working directory
will be automatically set to /app.

.PARAMETER version
Specify the version of GPSBabel.  This is only used with the -cli, -gui, and
-clean options.  The default is "latest".  Valid versions are "1.5.0",
"1.5.1", "1.5.2", "1.5.3", "1.5.4", "1.6.0", "1.7.0", "1.8.0", "1.9.0",
"1.10.0", "latest", and "dev".

.PARAMETER cli
Run the gpsbabel CLI, passing all the remaining arguments.  
This option is exlusive with the -gui, -list and -clean options.

.PARAMETER gui
Run the gpsbabel GUI, passing all remaining arguments.
This option is exclusive with the -cli, -list and -clean options.

.PARAMETER list
This option will list the related containers and images for all versions
that existlocally.  This option is exclusive with the -cli, -gui and -clean
options.

.PARAMETER clean
This option will stop and remove an existing container for the given version
and delete the corresponding image.  This will save space but it will take
longer to use the image the next time.  This option is exclusive with the
-cli, -gui and -list options.

.EXAMPLE
.\run_gpsbabel.ps1 -gui
Runs the gpsbabel GUI.

.EXAMPLE
.\run_gpsbabel.ps1 -cli -- -D 1 -i gpx -f cp.gpx -o kml -F "cp converted.kml"
Runs the gpsbabel CLI with the command line parameters pass as the -cli option.

.EXAMPLE
.\run_gpsbabel.ps1 -list
Will list any existing related containers and images.

.EXAMPLE
.\run_gpsbabel.ps1 -clean
Stop and delete the container and delete the image (for the specified version).

.LINK
https://hub.docker.com/r/tsteven4/gpsbabel

#>

param(
    # -version only allowed with clean, cli, gui.
    [Parameter(ParameterSetName = 'CleanSet')]
    [Parameter(ParameterSetName = 'CliSet')]
    [Parameter(ParameterSetName = 'GuiSet')]
    [ValidateSet("1.5.0", "1.5.1", "1.5.2", "1.5.3", "1.5.4", "1.6.0", "1.7.0", "1.8.0", "1.9.0", "1.10.0", "latest", "dev")]
    [string] $version = "latest",

    [Parameter(ParameterSetName = 'ListSet')]
    [switch] $list,

    [Parameter(ParameterSetName = 'CleanSet')]
    [switch] $clean,

    [Parameter(ParameterSetName = 'CliSet')]
    [switch] $cli,

    [Parameter(ParameterSetName = 'GuiSet')]
    [switch] $gui,

    [Parameter(ValueFromRemainingArguments = $true, ParameterSetName = 'CliSet')]
    [Parameter(ValueFromRemainingArguments = $true, ParameterSetName = 'GuiSet')]
    [string[]] $ProgramArgs
)

#Write-Output "Parameter set: $($PSCmdlet.ParameterSetName)"
#Write-Output "version = $version"
#Write-Output "list = $list"
#Write-Output "clean = $clean"

# we use a persistent container so the gpsbabel GUI state is saved and restored (in the container).
$ContainerName = "tsteven4_gpsbabel_${version}"
$ImageName = "tsteven4/gpsbabel:${version}"

if ($PSCmdlet.ParameterSetName -eq 'ListSet') {
    Write-Output "Running list for all versions"
    Write-Output "Containers:"
    docker ps --all --filter "name=tsteven4_gpsbabel_*" --format "table {{.Names}}\t{{.Image}}\t{{.Size}}"
    Write-Output ""
    Write-Output "Images:"
    docker image ls --all --filter "reference=tsteven4/gpsbabel:*" --format "table {{.Repository}}:{{.Tag}}\t{{.Size}}"
}
elseif ($PSCmdlet.ParameterSetName -eq 'CleanSet') {
    Write-Output "Running clean for version ${version}"
    Write-Output "Checking container ${ContainerName}"
    if (docker ps --all --quiet --filter "status=running" --filter "name=^${ContainerName}$") {
        Write-Output "  Stopping container ${ContainerName}"
        docker stop ${ContainerName} | Out-Null
    }
    if (docker ps --all --quiet --filter "name=^${ContainerName}$") {
        Write-Output "  Deleting container ${ContainerName}"
        docker rm ${ContainerName} | Out-Null
    }
    Write-Output "Checking image ${ImageName}"
    if (docker image ls --all --quiet ${ImageName}) {
        Write-Output "  Deleting image ${ImageName}"
        docker image rm ${ImageName} | Out-Null
    }
}
elseif ($PSCmdlet.ParameterSetName -eq 'CliSet' -or $PSCmdlet.ParameterSetName -eq 'GuiSet') {
    if (-not (docker ps --all --quiet --filter "name=^${ContainerName}$")) {
        Write-Output "Creating container ${ContainerName} from image ${ImageName}"
        $DesktopPath = [Environment]::GetFolderPath("Desktop")
        $cultureName = (Get-Culture).Name
        # Create the Docker container
        docker container create --quiet --interactive --tty `
            --name ${ContainerName} `
            --volume /run/desktop/mnt/host/wslg/.X11-unix:/tmp/.X11-unix `
            --volume /run/desktop/mnt/host/wslg:/mnt/wslg `
            --env DISPLAY=:0 `
            --env WAYLAND_DISPLAY=wayland-0 `
            --env XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir `
            --env PULSE_SERVER=/mnt/wslg/PulseServer `
            --env LANG=${cultureName} `
            --workdir /app `
            --volume ${DesktopPath}:/app `
            "${ImageName}" | Out-Null
    }
    
    # If necessary, start the container
    if (-not (docker ps --all --quiet --filter "status=running" --filter "name=^${ContainerName}$")) {
        docker start ${ContainerName} | Out-Null
    }
        
    #if ($ProgramArgs) { $ProgramArgs | ForEach-Object { Write-Output " - $_" } } else { Write-Output "No Program args collected." }

    switch ($PSCmdlet.ParameterSetName) {
        'CliSet' {
            Write-Output "Running CLI version ${version}"
            $baseArgs = @('exec', '--interactive', '--tty', $ContainerName, 'gpsbabel')
            $allArgs = $baseArgs + $ProgramArgs
    
            Write-Output ('gpsbabel ' + ($ProgramArgs | ForEach-Object { if ($_ -match '\s') { '"{0}"' -f $_ } else { $_ } }) -join ' ')
            & docker @allArgs
            break
        }
        'GuiSet' {
            Write-Output "Running GUI version ${version}"
            $baseArgs = @('exec', '--detach', '--interactive', '--tty', $ContainerName, 'gpsbabelfe')
            $allArgs = $baseArgs + $ProgramArgs

            & docker @allArgs
            break
        }
    }
}
