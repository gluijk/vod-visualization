# Visualización de patrones de consumo de series VoD
# www.datosimagensonido.com

library(data.table)
library(png)


# Consumos de series VoD con al menos 2 views por usuario
DT=fread("ConsumosVoD.txt", header=T, sep='\t', dec=",", stringsAsFactors=F)

# Ordenamos por Cliente -> Serie -> Tiempo -> Episodio
DT=DT[order(NUM_CLIENTE, TITULO_SERIE, TIEMPO_TRAMO10MIN, CAPITULO_SERIE),]
DT$ANGLE=DT$TIEMPO_TRAMO10MIN-min(DT$TIEMPO_TRAMO10MIN)
DT$ANGLE=DT$ANGLE/max(DT$ANGLE)*2*pi

LARGO=944  # +80px de márgenes = 1024px
CENTRO=LARGO/2
NUMCAPS=15  # Consideramos hasta episodio 15
RatioR=1.4
bwTh=9  # Umbral binge watching: 9 tramos de 10min = 90min

imgbw=NewBitmap(dimx=LARGO, dimy=LARGO, val=0)
img0d=imgbw
img1d=imgbw
img7d=imgbw
imgre=imgbw

FINAL=nrow(DT)
Nbw=0
N0d=0
N1d=0
N7d=0
Nre=0
Nnotrazable=0

fh=10/60  # Factor conversión min -> horas
f=fh/24  # Factor conversión min -> días

i=1
CLIpre=DT$NUM_CLIENTE[i]
TITpre=DT$TITULO_SERIE[i]
R=CENTRO*(1-(DT$CAPITULO_SERIE[i]-1)/(NUMCAPS*RatioR))
xpre=R*cos(DT$ANGLE[i])
ypre=R*sin(DT$ANGLE[i])
i=i+1

while (i <= FINAL) {
    # Bucle para cada par Cliente/Serie
    while (CLIpre==DT$NUM_CLIENTE[i] & TITpre==DT$TITULO_SERIE[i]) {
        R=CENTRO*(1-(DT$CAPITULO_SERIE[i]-1)/(NUMCAPS*RatioR))
        xpos=R*cos(DT$ANGLE[i])
        ypos=R*sin(DT$ANGLE[i])
        if (DT$CAPITULO_SERIE[i] == DT$CAPITULO_SERIE[i-1]+1) {  # episodios consecutivos
            
            HORAINI=as.integer(DT$TIEMPO_TRAMO10MIN[i-1]*fh) %% 24  # rango 0..23
            DIAINI=as.integer(DT$TIEMPO_TRAMO10MIN[i-1]*f)
            DIAFIN=as.integer(DT$TIEMPO_TRAMO10MIN[i]*f)
            
            if (DT$TIEMPO_TRAMO10MIN[i]-DT$TIEMPO_TRAMO10MIN[i-1] <= bwTh) {  # binge watching
                imgbw=DrawLine(imgbw, CENTRO+xpre, CENTRO+ypre, CENTRO+xpos, CENTRO+ypos)
                Nbw=Nbw+1
            } else if (DIAFIN == DIAINI & HORAINI > 5) {  # 0 días (pero sin binge watching)
                img0d=DrawLine(img0d, CENTRO+xpre, CENTRO+ypre, CENTRO+xpos, CENTRO+ypos) 
                N0d=N0d+1
            } else if (DIAFIN == DIAINI+1) {  # 1 día
                img1d=DrawLine(img1d, CENTRO+xpre, CENTRO+ypre, CENTRO+xpos, CENTRO+ypos) 
                N1d=N1d+1
            } else if (DIAFIN == DIAINI+7) {  # 7 días
                img7d=DrawLine(img7d, CENTRO+xpre, CENTRO+ypre, CENTRO+xpos, CENTRO+ypos) 
                N7d=N7d+1
            } else {  # Resto: episodios consecutivos pero con lapso <> bw, 0, 1, 7
                imgre=DrawLine(imgre, CENTRO+xpre, CENTRO+ypre, CENTRO+xpos, CENTRO+ypos)
                Nre=Nre+1
            }
        } else {  # Resto: episodios no consecutivos y/o en orden inverso
            imgre=DrawLine(imgre, CENTRO+xpre, CENTRO+ypre, CENTRO+xpos, CENTRO+ypos)
            Nre=Nre+1
        }

        xpre=xpos
        ypre=ypos
        i=i+1
    }
    Nnotrazable=Nnotrazable+1  # View no trazable: cambio de cliente y/o de serie
    
    CLIpre=DT$NUM_CLIENTE[i]
    TITpre=DT$TITULO_SERIE[i]
    R=CENTRO*(1-(DT$CAPITULO_SERIE[i]-1)/(NUMCAPS*RatioR))
    xpre=R*cos(DT$ANGLE[i])
    ypre=R*sin(DT$ANGLE[i])
    i=i+1    
}


# Guardamos gráficos normalizados separadamente
SaveBitmap(imgbw/max(imgbw), name='vodbw')
SaveBitmap(img0d/max(img0d), name='vod0dia')
SaveBitmap(img1d/max(img1d), name='vod1dia')
SaveBitmap(img7d/max(img7d), name='vod7dias')
SaveBitmap(imgre/max(imgre), name='vodrest')
