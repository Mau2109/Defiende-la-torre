import pygame
import sys
import socket
import json

# =============================
# CLIENTE HASKELL
# =============================
class HaskellClient:
    def __init__(self, host="localhost", port=3000):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            self.sock.connect((host, port))
            print("Conectado al servidor Haskell")
        except ConnectionRefusedError:
            print("No se pudo conectar al servidor Haskell en", host, port)
            sys.exit(1)

    def step(self):
        """Pide el siguiente estado al servidor Haskell"""
        try:
            self.sock.sendall(b"step\n")  # enviar comando step
        except BrokenPipeError:
            print("Conexión rota con el servidor Haskell")
            return None

        try:
            data = self.sock.recv(65536)
        except ConnectionResetError:
            print("Servidor Haskell cerró la conexión")
            return None

        if not data:
            print("Servidor Haskell no envió datos")
            return None

        try:
            return json.loads(data.decode())
        except json.JSONDecodeError:
            print("ERROR: no se pudo decodificar JSON:")
            print(data)
            return None

    def close(self):
        """Cierra la conexión correctamente"""
        try:
            self.sock.sendall(b"quit\n")
        except Exception:
            pass
        self.sock.close()
        print("Conexión cerrada con Haskell")

# =============================
# CONFIGURACIÓN INICIAL
# =============================
pygame.init()
WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Tower Defense - Python GUI")
clock = pygame.time.Clock()

# Conectar a Haskell
haskell = HaskellClient()

# =============================
# ESTADO INICIAL
# =============================
state = haskell.step()  # pedir estado inicial
if state is None:
    print("No se pudo obtener el estado inicial")
    haskell.close()
    pygame.quit()
    sys.exit(1)

# =============================
# LOOP PRINCIPAL
# =============================
while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            haskell.close()
            pygame.quit()
            sys.exit()

    # Obtener siguiente estado desde Haskell
    state = haskell.step()
    if state is None:
        print("Servidor desconectado o error en la comunicación")
        haskell.close()
        pygame.quit()
        sys.exit(1)

    # Extraer listas del estado
    enemies = state.get("gsEnemies", [])
    towers = state.get("gsTowers", [])
    projectiles = state.get("gsProjectiles", [])

    # Limpiar pantalla
    screen.fill((30, 30, 30))

    # =============================
    # DIBUJAR ENEMIGOS
    # =============================
    for e in enemies:
        x, y = e["enemyPos"]
        pygame.draw.circle(screen, (255, 0, 0), (int(x), int(y)), 10)

    # =============================
    # DIBUJAR TORRES
    # =============================
    for t in towers:
        x, y = t["towerPos"]
        pygame.draw.circle(screen, (0, 0, 255), (int(x), int(y)), 15)

    # =============================
    # DIBUJAR PROYECTILES
    # =============================
    for p in projectiles:
        x, y = p["projPos"]
        pygame.draw.circle(screen, (255, 255, 0), (int(x), int(y)), 5)

    pygame.display.flip()
    clock.tick(60)
