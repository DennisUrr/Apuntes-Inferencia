##################################
#     Laboratorio N°1 Redes      #
##################################

# Integrantes
# Dennis Urrutia
# Rodolfo Ugarte


# Dudas
# Como definir el limite de la frecuencia en el dominio de la frecuencia
# Comprobar que el grafico del espectograma este bien
# Como obtener la frecuencia de corte, a que se refiere con distintos parametros de entrada

#Se importan librerias
import numpy as np
from numpy import real
import matplotlib.pyplot as plt
import scipy.io.wavfile as waves
import scipy as sc
from scipy.signal import butter, lfilter
 
def graficarSonido(tiempo,sonido):
    plt.figure()
    plt.plot(tiempo,sonido)
    plt.title("Senial del sonido")
    plt.xlabel("t(s)")
    plt.ylabel("f(t) | sonido(t)")
    plt.grid()
    plt.show()
    
def graficarDominioFrecuencia(frecuencia,transformada):
    plt.figure()
    plt.plot(frecuencia,abs(transformada))
    plt.title("Senial en el dominio de la frecuencia")
    plt.xlabel("Frecuencia (Hz)")
    plt.ylabel("|F(w)|")
    plt.grid()
    plt.show() 
    
def graficarAntitransformada(tiempo,antitransformada):
    plt.figure()
    plt.plot(tiempo,real(antitransformada))
    plt.title("Antitransformada")
    plt.xlabel("Tiempo(s)")
    plt.ylabel("f(t)")
    plt.grid()
    plt.show()
    
def filtroPasoAlto(frec,frecCorte,sonido,orden):
    # expresadas como la fracción de la frecuencia Nyquist, que es la mitad de la frecuencia de muestreo
    #Es la frecuencia máxima que puede estar presente en la senial analógica sin que se produzca «aliasing»
    # en la senial discretizada.1​ 
    nyq = 0.5*frec
    frecCorteNormal = frecCorte/nyq 
    b,a = butter(orden,frecCorteNormal, btype = "highpass", analog = False)
    senialFiltrada = lfilter(b,a,sonido)
    return senialFiltrada

def filtroPasoBajo(frec,frecCorte,sonido,orden):
    nyq = 0.5*frec
    frecCorteNormal = frecCorte/nyq 
    b,a = butter(orden,frecCorteNormal, btype = "lowpass", analog = False)
    senialFiltrada = lfilter(b,a,sonido)
    return senialFiltrada

def filtroPasoBanda(frec,frecCorte,sonido,orden,lowcut,highcut):
    nyq = 0.5*frec
    low = lowcut/nyq
    high = highcut/nyq
    b, a = butter(orden,[low, high],btype='band')   
    senialFiltrada = lfilter(b,a,sonido)
    return senialFiltrada

def graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo):
    plt.figure()
    plt.plot(tiempo, senialOriginal, 'b-', label='Sonido Original')
    plt.plot(tiempo,senialFiltrada, 'g-', linewidth=2, label='Sonido Filtrado')
    plt.xlabel('Tiempo(s)')
    plt.ylabel("f(t)")
    plt.title(tipoFiltro)
    plt.grid()
    plt.legend()
    plt.show()
    
#Bloque principal
def main():
    #Se establece el nombre del archivo a leer
    archivo = 'prueba.wav'
    
    # frec: es la frecuencia de los datos por segundo, frecuencia de muestreo del sonido en PCM (MODULACION POR IMPULSOS CODIFICADOS) [hz] (tasa de muestreo)
    # senialOriginal: contiene los datos de la senial
    frec, senialOriginal = waves.read(archivo)
    
    #Se calcula el intervalo de muestreo
    dt = 1/frec  
    
    #Se calcula el largo del arreglo sonido
    largoSonido = len(senialOriginal)
            
    #Se crea un vector tiempo 
    #Limite = largoSonido / frec o largoSonido * dt en segundos
    tiempo = np.arange(0,dt*largoSonido,dt)
            
    #Se grafica la senial original
    graficarSonido(tiempo,senialOriginal)
    
    #Se calcula la transformada de Fourier
    transformada = sc.fft.fft(senialOriginal)

    #Se crea un vector de frecuencia
    frecTransformada = sc.fft.fftfreq(largoSonido,dt)
    
    #Se grafica la transformada, obteniendo el dominio de la frecuencia
    graficarDominioFrecuencia(frecTransformada,transformada)
    
    #Se calcula la antitransformada
    antitransformada = sc.fft.ifft(transformada)
    
    #Se grafica la antitransformada obteniendo algo similar a la senial original
    graficarAntitransformada(tiempo,antitransformada)
    
    #Se grafica el espectograma
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialOriginal, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma")
    
    #Se crean los filtros
    
    # Orden: El orden de un filtro describe el grado de aceptación o rechazo de frecuencias por arriba o por debajo,
    # de la respectiva frecuencia de corte.
    
    # Frec de Corte: es un límite en la respuesta frecuencial de un sistema en el cual la energía que 
    # fluye a través del mismo se comienza a reducir (atenuar or reflejar) en lugar de pasar a través de el. 
    
 
    
###############################################################################################   

    tipoFiltro = "Filtro Paso Alto"
    
    #el taps hace que se marque mas el area azul, debe ser impar
    h = sc.signal.firwin(101,1000, fs=frec, pass_zero = False)
    senialFiltrada = sc.signal.lfilter(h, [1.0], senialOriginal)
    graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo)
    
    archivo='FiltroPasoAlto.wav'
    waves.write(archivo,frec,senialFiltrada)
    
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialFiltrada, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma Senial Paso Alto")
    
    transformada = sc.fft.fft(senialFiltrada)
    plt.figure()
    plt.plot(tiempo,abs(transformada))
    plt.title("Transformada Filtro Paso Alto")
    plt.xlabel("Tiempo(s)")
    plt.ylabel("f(t)")
    plt.grid()
    plt.show()
    

################################################################################################ 

    tipoFiltro = "Filtro Paso Bajo"
    h = sc.signal.firwin(1001,1000, fs = frec, pass_zero = True)

    senialFiltrada = sc.signal.lfilter(h, [1.0], senialOriginal)
    graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo)
    
    archivo='FiltroPasoBajo.wav'
    waves.write(archivo,frec,senialFiltrada)
    
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialFiltrada, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma Senial Paso Bajo")
    
    transformada = sc.fft.fft(senialFiltrada)
    
    plt.figure()
    plt.plot(tiempo,abs(transformada))
    plt.title("Transformada Filtro Paso Bajo")
    plt.xlabel("Tiempo(s)")
    plt.ylabel("f(t)")
    plt.grid()
    plt.show()


###############################################################################################

    tipoFiltro = "Filtro Paso Banda"
    h = sc.signal.firwin(1001,cutoff = [500,2000],fs = frec, pass_zero = False)
    senialFiltrada = sc.signal.lfilter(h,[1.0], senialOriginal)
    graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo)
    
    archivo='FiltroPasoBanda.wav'
    waves.write(archivo,frec,senialFiltrada)
    
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialFiltrada, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma Senial Paso Banda")
    
    transformada = sc.fft.fft(senialFiltrada)
    
    plt.figure()
    plt.plot(tiempo,abs(transformada))
    plt.title("Transformada Filtro Paso Banda")
    plt.xlabel("Tiempo(s)")
    plt.ylabel("f(t)")
    plt.grid()
    plt.show()
    
################################################################################################
    
    '''
    #Parametros
    frecCorte = 1000
    orden = 10
    #Filtro paso bajo
    tipoFiltro = "Filtro Paso Bajo"
    senialFiltrada = filtroPasoBajo(frec,frecCorte,senialOriginal,orden)
    graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo)
    archivo='FiltroPasoBajo.wav'
    waves.write(archivo,frec,senialFiltrada)
    
    #Se grafica el espectograma
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialFiltrada, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma Senial Paso Bajo")
    
    #Parametros
    frecCorte = 1500
    orden = 10
    #Filtro paso alto
    tipoFiltro = "Filtro Paso Alto"
    senialFiltrada = filtroPasoAlto(frec,frecCorte,senialOriginal,orden)
    graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo)
    archivo='FiltroPasoAlto.wav'
    waves.write(archivo,frec,senialFiltrada)
    
    #Se grafica el espectograma
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialFiltrada, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma Senial Paso Alto")
    
    #Filtro paso banda
    orden = 10
    lowcut = 500
    highcut = 1000
    tipoFiltro = "Filtro Paso Banda"
    senialFiltrada = filtroPasoBanda(frec,frecCorte,senialOriginal,orden,lowcut,highcut)
    graficarFiltro(tipoFiltro,senialOriginal,senialFiltrada,tiempo)
    archivo='FiltroPasoBanda.wav'
    waves.write(archivo,frec,senialFiltrada)
    
    #Se grafica el espectograma
    frecuenciaEspectograma, tiempoEspectograma, espectograma = sc.signal.spectrogram(senialFiltrada, frec)
    plt.pcolormesh(tiempoEspectograma, frecuenciaEspectograma, np.log10(espectograma), shading='auto')
    plt.xlabel('Tiempo (s)')
    plt.ylabel('Frecuencia (Hz)')
    plt.title("Espectograma Senial Paso Banda")
    
    '''
    
    
    
#################################################################

#https://stackoverrun.com/es/q/6907815
#https://stackoverrun.com/es/q/3237564
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.io.wavfile.write.html
#http://blog.espol.edu.ec/estg1003/audio-archivo-wav-en-python/
#https://stackoverrun.com/es/q/2011745
#https://matplotlib.org/3.1.1/api/_as_gen/matplotlib.pyplot.specgram.html
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.fftpack.fftfreq.html
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.butter.html
#https://es.wikipedia.org/wiki/Frecuencia_de_Nyquist
#https://stackoverrun.com/es/q/5197778
#https://pybonacci.org/2012/09/29/transformada-de-fourier-discreta-en-python-con-scipy/

#################################################################
    '''
    n = len(sonido)
    taps = signal.firwin(n, cutoff = 0.3, window = "hamming")
    w,h = freqz(taps)
    filtro1 = lfilter(taps, 20*np.log10(h), sonido)
    
    archivo='Filtro1.wav'
    waves.write(archivo, 2.0,filtro1)
    '''
    

#Se llama al bloque principal
if __name__ == "__main__":
    main()
