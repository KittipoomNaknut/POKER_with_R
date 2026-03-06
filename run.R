# ============================================
# POKER LAN GAME - RUN SCRIPT
# ============================================
# 
# วิธีใช้:
# 1. เปิด R/RStudio
# 2. setwd("path/to/poker-lan")
# 3. source("run.R")
#
# หรือ:
# Rscript run.R
# ============================================

# ==========================================
# Settings (แก้ตรงนี้ได้)
# ==========================================

HOST <- "0.0.0.0"       # อย่าแก้ (ให้คนอื่นเข้าได้)
PORT <- 3838            # เปลี่ยนได้ถ้า port ชน
LAUNCH_BROWSER <- TRUE  # เปิด browser อัตโนมัติ

# ==========================================
# Functions
# ==========================================

#' Get host IP address
get_host_ip <- function() {
  os <- Sys.info()["sysname"]
  
  ip <- tryCatch({
    if (os == "Windows") {
      # Windows
      ip_info <- system("ipconfig", intern = TRUE)
      ip_line <- grep("IPv4", ip_info, value = TRUE)[1]
      if (is.na(ip_line)) {
        return("Unable to detect")
      }
      ip <- trimws(sub(".*: ", "", ip_line))
      ip
      
    } else if (os == "Darwin") {
      # macOS
      ip <- system("ipconfig getifaddr en0 2>/dev/null", intern = TRUE)
      if (length(ip) == 0) {
        ip <- system("ipconfig getifaddr en1 2>/dev/null", intern = TRUE)
      }
      if (length(ip) == 0) {
        return("Unable to detect")
      }
      trimws(ip)
      
    } else {
      # Linux
      ip <- system("hostname -I 2>/dev/null", intern = TRUE)
      if (length(ip) == 0) {
        return("Unable to detect")
      }
      trimws(strsplit(ip, " ")[[1]][1])
    }
  }, error = function(e) {
    "Unable to detect"
  })
  
  return(ip)
}

#' Check if required packages are installed
check_packages <- function() {
  required <- c("shiny", "shinyjs")
  missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
  
  if (length(missing) > 0) {
    cat("❌ Missing packages:", paste(missing, collapse = ", "), "\n")
    cat("\n")
    cat("Install with:\n")
    cat(paste0('install.packages(c("', paste(missing, collapse = '", "'), '"))\n'))
    cat("\n")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Check if firewall might be blocking
check_firewall_warning <- function() {
  os <- Sys.info()["sysname"]
  
  cat("⚠️  FIREWALL WARNING:\n")
  cat("   Make sure port", PORT, "is allowed through your firewall!\n")
  cat("\n")
  
  if (os == "Windows") {
    cat("   Windows:\n")
    cat("   1. Open PowerShell as Administrator\n")
    cat("   2. Run:\n")
    cat('      New-NetFirewallRule -DisplayName "Poker Game" \\\n')
    cat('        -Direction Inbound -Protocol TCP \\\n')
    cat(paste0('        -LocalPort ', PORT, ' -Action Allow\n'))
    
  } else if (os == "Darwin") {
    cat("   Mac:\n")
    cat("   System Preferences → Security & Privacy → Firewall\n")
    cat("   → Firewall Options → Add RStudio → Allow\n")
    
  } else {
    cat("   Linux:\n")
    cat(paste0("   sudo ufw allow ", PORT, "/tcp\n"))
    cat("   sudo ufw reload\n")
  }
  
  cat("\n")
}

# ==========================================
# Main Script
# ==========================================

# Clear console
cat("\014")

# ASCII Art
cat("\n")
cat("  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣\n")
cat("                                      \n")
cat("    🃏  POKER LAN GAME  🃏           \n")
cat("         Texas Hold'em                \n")
cat("                                      \n")
cat("  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣  ♠♥♦♣\n")
cat("\n")

# Check packages
if (!check_packages()) {
  cat("\n")
  cat("❌ Cannot start - missing packages!\n")
  cat("\n")
  quit(save = "no", status = 1)
}

# Load libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
})

# Get IP
host_ip <- get_host_ip()

# Display info
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("📡 SERVER INFORMATION\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")
cat(paste0("  Binding:  ", HOST, "\n"))
cat(paste0("  Port:     ", PORT, "\n"))
cat(paste0("  Host IP:  ", host_ip, "\n"))
cat("\n")

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🎮 HOW TO PLAY\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")

# Host URL
cat("👤 HOST (You):\n")
cat(paste0("   Open browser → http://localhost:", PORT, "\n"))
cat("\n")

# Players URL
cat("👥 PLAYERS (Others):\n")
if (host_ip != "Unable to detect") {
  cat(paste0("   Open browser → http://", host_ip, ":", PORT, "\n"))
  cat("\n")
  cat("   📱 Share this URL with players!\n")
  cat("   💡 Make QR code: qr-code-generator.com\n")
} else {
  cat("   ⚠️  Could not detect IP automatically\n")
  cat("   ℹ️  Find your IP:\n")
  cat("      Windows: ipconfig\n")
  cat("      Mac:     ifconfig | grep inet\n")
  cat("      Linux:   hostname -I\n")
  cat(paste0("   Then share: http://YOUR_IP:", PORT, "\n"))
}
cat("\n")

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("⚙️  REQUIREMENTS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")
cat("  ✅ All players on same WiFi/LAN\n")
cat("  ✅ Firewall allows port", PORT, "\n")
cat("  ✅ Minimum 2 players to start\n")
cat("\n")

# Firewall warning
check_firewall_warning()

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🚀 STARTING SERVER...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")
cat("  Press Ctrl+C to stop server\n")
cat("\n")

# Small delay for dramatic effect
Sys.sleep(1)

# Run app
tryCatch({
  runApp(
    appDir = ".",
    host = HOST,
    port = PORT,
    launch.browser = LAUNCH_BROWSER
  )
}, error = function(e) {
  cat("\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("❌ ERROR\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("\n")
  cat("Error message:\n")
  cat(paste0("  ", e$message, "\n"))
  cat("\n")
  
  if (grepl("already in use", e$message, ignore.case = TRUE)) {
    cat("💡 Port", PORT, "is already in use!\n")
    cat("\n")
    cat("Solutions:\n")
    cat("  1. Close other R/RStudio instances\n")
    cat("  2. Change PORT in run.R\n")
    cat("  3. Kill the process using the port\n")
    cat("\n")
  }
  
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("\n")
})

# Cleanup message
cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("👋 SERVER STOPPED\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")
cat("Thanks for playing! 🃏\n")
cat("\n")

