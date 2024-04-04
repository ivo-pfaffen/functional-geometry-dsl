Estas son instrucciones para instalar en Linux (recomendado) y en Windows. Si tienen otro sistema operativo y no pueden instalar fácilmente las bibliotecas les recomendamos instalar una máquina virtual con Linux.

El lab está pensado para hacer con gloss que es una interfaz para [(free)GLUT](https://github.com/freeglut/freeglut), que es una biblioteca para exponer OpenGL de manera uniforme en diferentes sistemas operativos. Alternativamente se puede instalar gloss para usar [GLFW](https://www.glfw.org/), ver más abajo.

Si tienen dificultades para instalar gloss pueden usar Lucid-SVG o Haha que no dependen de OpenGL.

Para más información sobre la forma de instalar GHC recomendamos consultar la [página oficial de GHCup](https://www.haskell.org/ghcup/install/).

# Linux (o macOS)

Estas instrucciones asumen que no usarán el manejador de paquetes de la distro que usen.

Ejecutar en una terminal (no requiere `sudo` ni que se hayan logueado como `root`):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

# Windows
Ejecutar en una terminal (PowerShell o Windows Terminal):

```
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```

Acepten todas las opciones por defecto, en particular asegurénse de instalar msys2. Una vez que termina la instalación necesitamos instalar freeglut. Para ello abrimos `mintty` (deberían tener un ícono en el escritorio).

Msys2 incluye el manejador de paquetes `pacman` (originalmente de ArchLinux), entonces instalar librerías y programas es muy sencillo. Dentro de `mintty` deben hacer:
```bash
pacman -S mingw-w64-x86_64-freeglut
 ```

# Gloss con GLFW

Si prefieren usar GLFW (no lo recomendamos excepto que tu sistema operativo no sea ni Linux ni Windows). Asegurénse de tener la biblioteca GLFW instalada previamente.

```bash
cabal install gloss --flags="GLFW -GLUT
```

**Aclaración**: Esto no fue testeado por la cátedra. La cátedra no hará ningún esfuerzo en que les funcione gloss nativamente. Si ven que no lo pueden hacer funcionar tienen dos alternativas:

1. Usar una máquina virtual. 
2. Hace el lab con la biblioteca haha o lucid-svg.

# Verificar que gloss está funcionando

En el directorio del repo:

```bash
cabal test gloss
```

# Verificar que haha está funcionando

En el directorio del repo:

```bash
cabal test haha
```

El test de haha debería mostrarles un rectángulo hecho con asteriscos. Si ven ese rectángulo el test funcionó; no le presten atención al output del test que dice que falló.
