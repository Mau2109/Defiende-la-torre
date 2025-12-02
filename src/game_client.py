import pygame
import socket
import json
import sys
import time
from typing import Dict, List, Tuple, Optional

# Inicializar Pygame
pygame.init()

# Constantes
SCREEN_WIDTH = 800
SCREEN_HEIGHT = 600
FPS = 60

# Colores
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GREEN = (0, 255, 0)
RED = (255, 0, 0)
BLUE = (0, 100, 255)
YELLOW = (255, 255, 0)
GRAY = (128, 128, 128)
DARK_GREEN = (0, 128, 0)

class GameClient:
    def __init__(self, host='127.0.0.1', port=3000):
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Defiende la Torre")
        self.clock = pygame.time.Clock()
        self.running = True
        
        # Conexión con servidor Haskell
        print(f"Intentando conectar a {host}:{port}...")
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.settimeout(10.0)
        
        try:
            self.socket.connect((host, port))
            print(f"✓ Conectado al servidor Haskell")
        except Exception as e:
            print(f"✗ Error conectando al servidor: {e}")
            print("Asegúrate de que el servidor Haskell esté ejecutándose:")
            print("  cd ~/Documentos/septimo\\ semestre/Programacion_Funcional/Proyecto3/defiende-la-torre")
            print("  cabal run")
            sys.exit(1)
        
        # Estado del juego
        self.game_state = None
        self.selected_tower_type = "Basic"
        self.font = pygame.font.Font(None, 36)
        self.small_font = pygame.font.Font(None, 24)
        self.buffer = b''  # Buffer para recibir datos
        
        # Obtener estado inicial
        print("Esperando estado inicial del servidor...")
        time.sleep(0.5)  # Dar tiempo al servidor
        
        self.game_state = self.receive_state()
        
        if not self.game_state:
            print("✗ Error: No se recibió el estado inicial")
            sys.exit(1)
        
        print("✓ Estado inicial recibido")
        print(f"  - Camino: {len(self.game_state.get('rspPath', []))} puntos")
        print(f"  - Vidas: {self.game_state.get('rspLives', 0)}")
        print(f"  - Oro: {self.game_state.get('rspMoney', 0)}")
        print(f"  - Oleada: {self.game_state.get('rspWaveNumber', 0)}")
    
    def send_command(self, command: Dict) -> bool:
        """Enviar comando al servidor Haskell"""
        try:
            msg = json.dumps(command) + '\n'
            self.socket.sendall(msg.encode('utf-8'))
            return True
        except Exception as e:
            print(f"✗ Error enviando comando: {e}")
            self.running = False
            return False
    
    def receive_state(self) -> Optional[Dict]:
        """Recibir estado del juego desde Haskell"""
        try:
            # Leer datos hasta encontrar una línea completa
            while b'\n' not in self.buffer:
                chunk = self.socket.recv(4096)
                if not chunk:
                    print("✗ Servidor cerró la conexión")
                    return None
                self.buffer += chunk
            
            # Extraer la primera línea completa
            line, self.buffer = self.buffer.split(b'\n', 1)
            
            if not line.strip():
                return self.receive_state()  # Línea vacía, intentar de nuevo
            
            # Decodificar JSON
            try:
                state = json.loads(line.decode('utf-8'))
                return state
            except json.JSONDecodeError as e:
                print(f"✗ Error decodificando JSON: {e}")
                print(f"  Datos recibidos: {line[:100]}")
                return None
                
        except socket.timeout:
            print("✗ Timeout esperando respuesta del servidor")
            return None
        except Exception as e:
            print(f"✗ Error recibiendo estado: {e}")
            return None
    
    def handle_events(self):
        """Manejar eventos de Pygame"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.running = False
            
            elif event.type == pygame.MOUSEBUTTONDOWN:
                x, y = event.pos
                
                # Botones de UI
                if y > SCREEN_HEIGHT - 80:
                    self.handle_ui_click(x, y)
                else:
                    # Colocar torre en el mapa
                    self.place_tower(x, y)
            
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    # Iniciar oleada
                    self.send_command({"type": "StartWave"})
                    self.game_state = self.receive_state()
                
                elif event.key == pygame.K_1:
                    self.selected_tower_type = "Basic"
                elif event.key == pygame.K_2:
                    self.selected_tower_type = "Sniper"
                elif event.key == pygame.K_3:
                    self.selected_tower_type = "Splash"
    
    def handle_ui_click(self, x: int, y: int):
        """Manejar clicks en la interfaz de usuario"""
        # Botón de iniciar oleada
        if 10 <= x <= 160 and SCREEN_HEIGHT - 70 <= y <= SCREEN_HEIGHT - 30:
            self.send_command({"type": "StartWave"})
            self.game_state = self.receive_state()
        
        # Botones de selección de torre
        button_y = SCREEN_HEIGHT - 70
        if button_y <= y <= SCREEN_HEIGHT - 30:
            if 180 <= x <= 280:
                self.selected_tower_type = "Basic"
            elif 290 <= x <= 390:
                self.selected_tower_type = "Sniper"
            elif 400 <= x <= 500:
                self.selected_tower_type = "Splash"
    
    def place_tower(self, x: int, y: int):
        """Colocar torre en el mapa"""
        command = {
            "type": "PlaceTower",
            "cmdX": float(x),
            "cmdY": float(y),
            "cmdType": self.selected_tower_type
        }
        self.send_command(command)
        self.game_state = self.receive_state()
    
    def update(self, dt: float):
        """Actualizar estado del juego"""
        if not self.game_state:
            return
        
        # Limitar dt para evitar saltos grandes
        dt = min(dt, 0.1)
        
        command = {
            "type": "Tick",
            "cmdDeltaTime": dt
        }
        
        if self.send_command(command):
            new_state = self.receive_state()
            if new_state:
                self.game_state = new_state
            else:
                print("⚠ No se recibió estado del servidor")
                self.running = False
    
    def draw(self):
        """Renderizar todo"""
        self.screen.fill(WHITE)
        
        if not self.game_state:
            # Mostrar mensaje de carga
            loading_text = self.font.render("Conectando al servidor...", True, BLACK)
            self.screen.blit(loading_text, (SCREEN_WIDTH//2 - 150, SCREEN_HEIGHT//2))
            pygame.display.flip()
            return
        
        # Dibujar camino
        self.draw_path()
        
        # Dibujar torres
        self.draw_towers()
        
        # Dibujar enemigos
        self.draw_enemies()
        
        # Dibujar proyectiles
        self.draw_projectiles()
        
        # Dibujar UI
        self.draw_ui()
        
        pygame.display.flip()
    
    def draw_path(self):
        """Dibujar el camino por donde van los enemigos"""
        path = self.game_state.get('rspPath', [])
        
        if not path:
            # Debug: mostrar mensaje si no hay camino
            no_path_text = self.small_font.render("No path data", True, RED)
            self.screen.blit(no_path_text, (10, 50))
            return
        
        if len(path) > 1:
            # Dibujar línea del camino más gruesa
            try:
                pygame.draw.lines(self.screen, GRAY, False, path, 25)
                # Borde del camino
                pygame.draw.lines(self.screen, BLACK, False, path, 3)
            except Exception as e:
                print(f"Error dibujando camino: {e}")
                print(f"Path data: {path[:5]}...")
            
            # Marcar inicio y fin
            if path:
                start_pos = (int(path[0][0]), int(path[0][1]))
                end_pos = (int(path[-1][0]), int(path[-1][1]))
                
                # Círculo de inicio (verde)
                pygame.draw.circle(self.screen, GREEN, start_pos, 20)
                pygame.draw.circle(self.screen, BLACK, start_pos, 20, 3)
                
                # Círculo de fin (rojo)
                pygame.draw.circle(self.screen, RED, end_pos, 20)
                pygame.draw.circle(self.screen, BLACK, end_pos, 20, 3)
                
                # Etiquetas
                start_text = self.small_font.render("START", True, WHITE)
                end_text = self.small_font.render("END", True, WHITE)
                self.screen.blit(start_text, (start_pos[0] - 25, start_pos[1] - 35))
                self.screen.blit(end_text, (end_pos[0] - 20, end_pos[1] - 35))
                
                # Debug: mostrar cantidad de puntos y rango de coordenadas
                min_x = min(p[0] for p in path)
                max_x = max(p[0] for p in path)
                min_y = min(p[1] for p in path)
                max_y = max(p[1] for p in path)
                
                debug_text = self.small_font.render(
                    f"Path: {len(path)} pts | X:[{int(min_x)},{int(max_x)}] Y:[{int(min_y)},{int(max_y)}]", 
                    True, BLACK
                )
                self.screen.blit(debug_text, (10, 50))
    
    def draw_towers(self):
        """Dibujar las torres"""
        towers = self.game_state.get('rspTowers', [])
        
        for tower in towers:
            x, y = int(tower['tdX']), int(tower['tdY'])
            tower_type = tower['tdType']
            
            # Color según tipo
            if tower_type == 'Basic':
                color = BLUE
            elif tower_type == 'Sniper':
                color = YELLOW
            else:  # Splash
                color = (255, 165, 0)  # Naranja
            
            # Dibujar torre
            pygame.draw.circle(self.screen, color, (x, y), 12)
            pygame.draw.circle(self.screen, BLACK, (x, y), 12, 2)
            
            # Nivel
            level_text = self.small_font.render(
                str(tower['tdLevel']), True, WHITE
            )
            self.screen.blit(level_text, 
                           (x - level_text.get_width()//2, 
                            y - level_text.get_height()//2))
    
    def draw_enemies(self):
        """Dibujar los enemigos"""
        enemies = self.game_state.get('rspEnemies', [])
        
        for enemy in enemies:
            x, y = int(enemy['edX']), int(enemy['edY'])
            hp_percent = enemy['edHP'] / enemy['edMaxHP']
            
            # Color según tipo
            enemy_type = enemy['edType']
            if 'Fast' in enemy_type:
                color = (0, 255, 255)  # Cyan
            elif 'Tank' in enemy_type:
                color = (150, 75, 0)   # Marrón
            else:
                color = RED
            
            # Dibujar enemigo
            pygame.draw.circle(self.screen, color, (x, y), 8)
            
            # Barra de vida
            bar_width = 20
            bar_height = 3
            bar_x = x - bar_width // 2
            bar_y = y - 15
            
            # Fondo rojo
            pygame.draw.rect(self.screen, RED, 
                           (bar_x, bar_y, bar_width, bar_height))
            # Vida actual en verde
            pygame.draw.rect(self.screen, GREEN, 
                           (bar_x, bar_y, int(bar_width * hp_percent), bar_height))
    
    def draw_projectiles(self):
        """Dibujar proyectiles"""
        projectiles = self.game_state.get('rspProjectiles', [])
        
        for proj in projectiles:
            x, y = int(proj['pdX']), int(proj['pdY'])
            pygame.draw.circle(self.screen, BLACK, (x, y), 3)
    
    def draw_ui(self):
        """Dibujar interfaz de usuario"""
        # Fondo de la UI
        pygame.draw.rect(self.screen, DARK_GREEN, 
                        (0, SCREEN_HEIGHT - 80, SCREEN_WIDTH, 80))
        
        # Información del jugador
        lives = self.game_state.get('rspLives', 0)
        money = self.game_state.get('rspMoney', 0)
        wave = self.game_state.get('rspWaveNumber', 0)
        wave_active = self.game_state.get('rspWaveActive', False)
        
        info_text = f"Vidas: {lives}  Dinero: ${money}  Oleada: {wave}"
        text_surface = self.small_font.render(info_text, True, WHITE)
        self.screen.blit(text_surface, (10, 10))
        
        # Botón de iniciar oleada
        wave_button_color = RED if wave_active else GREEN
        pygame.draw.rect(self.screen, wave_button_color, 
                        (10, SCREEN_HEIGHT - 70, 150, 40))
        wave_text = "OLEADA ACTIVA" if wave_active else "INICIAR OLEADA"
        button_text = self.small_font.render(wave_text, True, WHITE)
        self.screen.blit(button_text, (15, SCREEN_HEIGHT - 60))
        
        # Botones de torres
        tower_types = [
            ("Basic", BLUE, 180),
            ("Sniper", YELLOW, 290),
            ("Splash", (255, 165, 0), 400)
        ]
        
        for name, color, x_pos in tower_types:
            # Resaltar seleccionado
            if name == self.selected_tower_type:
                pygame.draw.rect(self.screen, WHITE, 
                               (x_pos - 2, SCREEN_HEIGHT - 72, 104, 44), 3)
            
            pygame.draw.rect(self.screen, color, 
                           (x_pos, SCREEN_HEIGHT - 70, 100, 40))
            tower_text = self.small_font.render(name, True, WHITE)
            self.screen.blit(tower_text, 
                           (x_pos + 10, SCREEN_HEIGHT - 60))
        
        # Instrucciones
        instructions = [
            "Click: Colocar torre | ESPACIO: Iniciar oleada",
            "1,2,3: Seleccionar tipo de torre"
        ]
        for i, inst in enumerate(instructions):
            inst_text = self.small_font.render(inst, True, BLACK)
            self.screen.blit(inst_text, (520, 10 + i * 25))
    
    def run(self):
        """Bucle principal del juego"""
        while self.running:
            dt = self.clock.tick(FPS) / 1000.0  # Delta time en segundos
            
            self.handle_events()
            self.update(dt)
            self.draw()
        
        self.socket.close()
        pygame.quit()

if __name__ == "__main__":
    client = GameClient()
    client.run()