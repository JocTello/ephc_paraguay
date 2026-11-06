*******************************************************************************
*                Tópicos de Econometría - UC (2026-Segundo Semestre)
*               Tema: Determinantes de la Informalidad Laboral en Paraguay
*                             Profesor: José Tello
*******************************************************************************

* Fijar el directorio de trabajo

*---> Abrir el archivo de datos: reg02_EPHC_3er Trim 2026.dta
use "archivos_dta_ine/e6d49-REG02_EPHC_3er Trim 2026.dta"

* Construcción de variables
egen hogar_id = group(upm nvivi nhoga)
tabulate p02
gen es_menor6  = (p02 <= 5)
gen es_menor15 = (p02 <= 14)

bysort hogar_id: egen tam_hogar = count(l02)
bysort hogar_id: egen n_menor6  = total(es_menor6)
bysort hogar_id: egen n_menor15 = total(es_menor15)

tabulate informalidad
gen informal = 1 if informalidad==1
replace informal = 0 if informalidad == 2
tabulate informal
label variable informal "Ocupado informal (1) / formal (0)"

replace añoest = . if añoest == 99
replace horab = . if horab == 999 | horab == 0
replace rama_pea = . if rama_pea == 99 | rama_pea == 1 
replace ocup_pea  = . if ocup_pea == 99
replace p09 = . if p09 == 9
replace tama_pea  = . if tama_pea == 99

sum p02
keep if inrange(p02, 15, 65)  /*Ahora restringimos la muestra a personas en edad de trabajar*/
gen edad = p02
gen edad_sq = edad^2
tabulate p06
gen mujer = (p06 == 6)

gen rural = (area == 6)
gen jefe = (p03 == 1)

gen est_civil = .
replace est_civil = 1 if p09 == 5
replace est_civil = 2 if inlist(p09, 1, 2)
replace est_civil = 3 if inlist(p09, 3, 6)
replace est_civil = 4 if p09 == 4
label define l_civil 1 "soltero" 2 "casado_unido" 3 "sep_div" 4 "viudo"
label values est_civil l_civil

tabulate estgeo
gen dep = floor(estgeo / 10)
tabulate dep
gen region = 4
replace region = 1 if dep == 0
replace region = 2 if dep == 11
replace region = 3 if dep == 10
label define l_reg 1 "Asuncion" 2 "Central" 3 "Alto_Parana" 4 "Resto_pais"
label values region l_reg

gen ln_horas     = ln(horab)
gen ln_tam_hogar = ln(tam_hogar)
gen hijos_menor6 = (n_menor6 > 0)

gen rama = rama_pea
label define l_rama 2 "Industria" 3 "Electr_gas_agua" 4 "Construccion" 5 "Comercio_hot_rest" 6 "Transporte_comunic" 7 "Finanzas_inmuebles" 8 "Servicios"
label values rama l_rama

gen ocup = ocup_pea
replace ocup = 1 if ocup_pea == 10
label define l_ocup 1 "Directivos_FFAA" 2 "Profesionales" 3 "Tecnicos" 4 "Oficina" 5 "Servicios_ventas" 6 "Agricultores" 7 "Oficios_artesanos" 8 "Operadores" 9 "No_calificados"
label values ocup l_ocup

label variable edad "Edad"
label variable edad_sq "Edad^2"
label variable mujer "Mujer"
label variable añoest "Años de estudio"
label variable rural "Área rural"
label variable jefe "Jefe/a de hogar"
label variable ln_tam_hogar "ln(Tamaño del hogar)"
label variable hijos_menor6 "Niños < 6 años en el hogar"
label variable n_menor15 "N° de menores de 15 en el hogar"
label variable ln_horas "ln(Horas trabajadas)"

* Modelo Logit Base 
logit informal edad edad_sq i.mujer añoest
estimates store m_base

* Modelo Logit Completo
logit informal edad edad_sq i.mujer añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup
estimates store m_logit

* Modelo Logit Ponderado
logit informal edad edad_sq i.mujer añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup, vce(cluster upm)
estimates store m_logit_robust

* Selección de modelos
lrtest m_logit m_base
estimates stats m_base m_logit

* Tests de Wald conjuntos por bloque
estimates restore m_logit
testparm i.est_civil
testparm i.region
testparm i.rama
testparm i.ocup
test edad edad_sq

* Efectos Marginales 

margins, dydx(mujer añoest rural jefe hijos_menor6 ln_horas ln_tam_hogar n_menor15)
marginsplot, horizontal xline(0) recast(scatter) title("Efectos marginales promedio") xtitle("Cambio en P(informal | X)")
graph export "resultados/f1_efectos_marginales.png", replace width(1200)

* Probabilidad estimada por edad y sexo

logit informal c.edad##c.edad i.mujer añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup, vce(cluster upm)

margins, dydx(edad)

margins mujer, at(edad = (15(5)75))
marginsplot, xdimension(edad) title("Probabilidad estimada de informalidad según edad y sexo") ytitle("Pr(informal | X)") legend(order(1 "Hombres" 2 "Mujeres"))
graph export "resultados/f2_prob_edad_sexo.png", replace width(1200)

* Clasificación

estimates restore m_logit
sum informal
local inf_m = r(mean)
estat classification
estat classification, cutoff(`inf_m')

* Modelo Logit Ponderado por Agrupación: upm

logit informal edad edad_sq i.mujer añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup [pweight = factor], vce(cluster upm)

estimates store m_logit_w

margins, dydx(edad mujer añoest rural jefe hijos_menor6 ln_horas)

* Efecto marginal de la educación en submuestras

quietly logit informal edad edad_sq añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup if mujer == 0, vce(cluster upm)
margins, dydx(añoest)
quietly logit informal edad edad_sq añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup if mujer == 1, vce(cluster upm)
margins, dydx(añoest)
quietly logit informal edad edad_sq añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup if rural == 0, vce(cluster upm)
margins, dydx(añoest)
quietly logit informal edad edad_sq añoest i.rural ib1.region i.jefe ib1.est_civil ln_tam_hogar i.hijos_menor6 n_menor15 ln_horas ib8.rama ib9.ocup if rural == 1, vce(cluster upm)
margins, dydx(añoest)
