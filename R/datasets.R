#' @title Cancer du sein
#'
#' @description Jeu de données cancer du sein.
#'
#' @docType data
#' @format Un objet \code{data.frame} avec trois variables et 62 observations :
#' \describe{
#'   \item{Traitement}{Factor w/ 3 levels, Type de traitement administré}
#'   \item{Age}{Numerical vector, Âge de la patiente au début du traitement}
#'   \item{Survie}{Numerical vector, Durée de survie de la patiente}
#' }
#' @source \emph{Modèle linéaire : Comparaison de groupes et régression} de B. Prum aux Éditions de l’INSERM, 1996.
"CancerSein"
#> [1] "CancerSein"

#' @title Challenger
#'
#' @description Jeu de données navette spatiale.
#'
#' @docType data
#' @format Un objet \code{data.frame} avec deux variables et 24 observations :
#' \describe{
#'   \item{Temperature}{Integer vector, Température au moment du décollage}
#'   \item{Defaillance}{Integer vector, Défaillance de l'un des \emph{o-ring}}
#' }
"chal"
#> [1] "chal"

#' @title Hotels
#'
#' @description Jeu de données hotels (nom des hotels dans la variable NOM).
#'
#' @docType data
#' @format Un objet \code{data.frame} avec neuf variables et 39 observations :
#' \describe{
#'   \item{NOM}{Factor w/ 39 levels, Nom de l'hôtel}
#'   \item{PAYS}{Factor w/ 5 levels, Pays où est situé l'hôtel}
#'   \item{ETOILE}{Integer vector, Catégorie de l'hôtel}
#'   \item{CONFORT}{Integer vector, Note de confort}
#'   \item{CHAMBRE}{Integer vector, Nombre de chambres}
#'   \item{CUISINE}{Integer vector, Note de la cuisine}
#'   \item{SPORT}{Integer vector, Note des instructures sprotives}
#'   \item{PLAGE}{Integer vector, Note de la plage}
#'   \item{PRIX}{Integer vector, Prix moyen pour une nuit}
#' }
"d_hotels"
#> [1] "d_hotels"

#' @title Hotels
#'
#' @description Jeu de données hotels (nom des hotels en rownames).
#'
#' @docType data
#' @format Un objet \code{data.frame} avec huit variables et 39 observations :
#' \describe{
#'   \item{PAYS}{Factor w/ 5 levels, Pays où est situé l'hôtel}
#'   \item{ETOILE}{Integer vector, Catégorie de l'hôtel}
#'   \item{CONFORT}{Integer vector, Note de confort}
#'   \item{CHAMBRE}{Integer vector, Nombre de chambres}
#'   \item{CUISINE}{Integer vector, Note de la cuisine}
#'   \item{SPORT}{Integer vector, Note des instructures sprotives}
#'   \item{PLAGE}{Integer vector, Note de la plage}
#'   \item{PRIX}{Integer vector, Prix moyen pour une nuit}
#' }
"d_hotels_n"
#> [1] "d_hotels_n"

#' @title MacDonald
#'
#' @description Jeu de données sur des aliments vendus au McDonald.
#'
#' @docType data
#' @format Un objet \code{data.frame} avec 24 variables et 260 observations :
#' \describe{
#'   \item{Category}{Factor w/ 9 levels, Type d'aliment}
#'   \item{Item}{Factor w/ 260 levels, Nom de l'aliment}
#'   \item{Serving.Size}{Factor w/ 107 levels, Taille}
#'   \item{Calories}{Integer vector, Calories}
#'   \item{Calories.from.Fat}{Integer vector, Calories dues à la graisse}
#'   \item{Total.Fat}{Numerical vector, Graisse totale}
#'   \item{Total.Fat....Daily.Value.}{Integer vector, \% de la quantité de graisse totale attendue par jour}
#'   \item{Saturated.Fat}{Numerical vector, Graisse saturée}
#'   \item{Saturated.Fat....Daily.Value.}{Integer vector, \% de la quantité de graisse saturée attendue par jour}
#'   \item{Trans.Fat}{Numerical vector, Trans.Fat}
#'   \item{Cholesterol}{Integer vector, Cholesterol}
#'   \item{Cholesterol....Daily.Value.}{Integer vector, \% de la quantité de cholesterol attendue par jour}
#'   \item{Sodium}{Integer vector, Sel}
#'   \item{Sodium....Daily.Value.}{Integer vector, \% de la quantité de sel attendue par jour}
#'   \item{Carbohydrates}{Integer vector, Carbohydrates}
#'   \item{Carbohydrates....Daily.Value.}{Integer vector, \% de la quantité de carbohydrates attendue par jour}
#'   \item{Dietary.Fiber}{Integer vector, Fibres}
#'   \item{Dietary.Fiber....Daily.Value.}{Integer vector, \% de la quantité de fibres attendue par jour}
#'   \item{Sugars}{Integer vector, Sucres}
#'   \item{Protein}{Integer vector, Protéines}
#'   \item{Vitamin.A....Daily.Value.}{Integer vector, \% de la quantité de vitamine A attendue par jour}
#'   \item{Vitamin.C....Daily.Value.}{Integer vector, \% de la quantité de vitamine C attendue par jour}
#'   \item{Calcium....Daily.Value.}{Integer vector, \% de la quantité de calcium attendue par jour}
#'   \item{Iron....Daily.Value.}{Integer vector, \% de la quantité de fer attendue par jour}
#' }
#' @details Ce jeu de données a été extrait de kaggle où il est possible de trouver une description complémentaire des variables.
#' @source Kaggle. 
"d_macdo"
#> [1] "d_macdo"

#' @title Elections presidentielles 2002 : premier tour
#'
#' @description Jeu de données sur le premier tour des élections présidentielles de 2002.
#' Les noms des lignes indiquent les régions.
#'
#' @docType data
#' @format Un objet \code{data.frame} avec 12 variables et 8 observations :
#' \describe{
#'   \item{Arlette.Laguiller}{Integer vector, Nombre de votes par région}
#'   \item{Dominique.Voynet}{Integer vector, Nombre de votes par région}
#'   \item{François.Bayrou}{Integer vector, Nombre de votes par région}
#'   \item{Frédéric.Nihous}{Integer vector, Nombre de votes par région}
#'   \item{Gérard.Schivardi}{Integer vector, Nombre de votes par région}
#'   \item{Jean.Marie.Le.Pen}{Integer vector, Nombre de votes par région}
#'   \item{José.Bové}{Integer vector, Nombre de votes par région}
#'   \item{Marie.George.Buffet}{Integer vector, Nombre de votes par région}
#'   \item{Nicolas.Sarkozy}{Integer vector, Nombre de votes par région}
#'   \item{Olivier.Besancenot}{Integer vector, Nombre de votes par région}
#'   \item{Philippe.de.Villiers}{Integer vector, Nombre de votes par région}
#'   \item{Ségolène.Royal}{Integer vector, Nombre de votes par région}
#' }
"d_pres2002"
#> [1] "d_pres2002"

#' @title Elections presidentielles 2007 : premier tour
#'
#' @description Jeu de données sur le premier tour des élections présidentielles de 2002.
#' Les noms des lignes indiquent les régions.
#'
#' @docType data
#' @format Un objet \code{data.frame} avec 12 variables et 23 observations :
#' \describe{
#'   \item{Sarkozy}{Integer vector, Nombre de votes par région}
#'   \item{Bayrou}{Integer vector, Nombre de votes par région}
#'   \item{Royal}{Integer vector, Nombre de votes par région}
#'   \item{Le.Pen}{Integer vector, Nombre de votes par région}
#'   \item{Besanc.}{Integer vector, Nombre de votes par région}
#'   \item{VIlliers}{Integer vector, Nombre de votes par région}
#'   \item{Voynet}{Integer vector, Nombre de votes par région}
#'   \item{Laguiller}{Integer vector, Nombre de votes par région}
#'   \item{Bove}{Integer vector, Nombre de votes par région}
#'   \item{Buffet}{Integer vector, Nombre de votes par région}
#'   \item{Nihous}{Integer vector, Nombre de votes par région}
#'   \item{Schivardi}{Integer vector, Nombre de votes par région}
#' }
"d_pres2007"
#> [1] "d_pres2007"

#' @title Tâches ménagènes
#' 
#' @description Répartition des tâches ménagèrs dans différents foyers. Les noms des lignes correspondent aux tâches ménagères.
#' 
#' @format Un objet \code{data.frame} avec 13 lignes et 4 variables :
#' \describe{
#'   \item{\code{Wife}}{Integer vector, Nombre de fois que la tâche est réalisée exclusivement par une femme}
#'   \item{\code{Alternating}}{Integer vector, Nombre de fois que la tâche est réalisée alternativement par une femme et un homme}
#'   \item{\code{Husband}}{Integer vector, Nombre de fois que la tâche est réalisée exclusivement par un homme}
#'   \item{\code{Jointly}}{Integer vector, Nombre de fois que la tâche est réalisée en même temps par une femme et un homme} 
#'}
"d_TM"
#> [1] "d_TM"

#' @title Lieux de vacances et occupation
#' 
#' @description Ce jeu de données croise le type de séjours de vacances avec l'occupation des individus. L'occupation des individus correspond au nom des lignes du jeu de données.
#' 
#' @format Un objet \code{data.frame} avec 8 lignes et 8 variables :
#' \describe{
#'   \item{\code{Hotel}}{Integer vector, Séjour à l'hotel}
#'   \item{\code{Location}}{Integer vector, Séjour en location}
#'   \item{\code{Res.Second}}{Integer vector, Séjour dans une résidence secondaire}
#'   \item{\code{Parents}}{Integer vector, Séjour chez les parents}
#'   \item{\code{Amis}}{Integer vector, Séjour chez des amis}
#'   \item{\code{Camping}}{Integer vector, Séjour au camping}
#'   \item{\code{Sej.org}}{Integer vector, Séjour organisé}
#'   \item{\code{Autres}}{Integer vector, Autres} 
#'}
"d_vac"
#> [1] "d_vac"

#' @title Zones de World of Warcraft
#' 
#' @description Ce jeu données est constitué des caractéristiques de zones du jeu World of Warcraft.
#' 
#' @format Un objet \code{data.frame} avec 160 lignes et 12 variables :
#' \describe{
#'   \item{\code{Continent}}{Factor w/ 7 levels, Nom du continent}
#'   \item{\code{Area}}{Factor w/ 12 levels, Nom de la région}
#'   \item{\code{Zone}}{Factor w/ 80 levels, Nom de la zone}
#'   \item{\code{Subzone}}{Factor w/ 38 levels, Nom de la sous-zone}
#'   \item{\code{Type}}{Factor w/ 8 levels, Type de zone}
#'   \item{\code{Size}}{Integer vector, Taille}
#'   \item{\code{Controlled}}{Factor w/ 5 levels, Faction contrôlant la zone}
#'   \item{\code{Min_req_level}}{Integer vector, Niveau minimum requis pour pouvoir accéder à la zone}
#'   \item{\code{Min_rec_level}}{Integer vector, Niveau minimum recommandé pour s'aventurer dans la zone}
#'   \item{\code{Max_rec_level}}{Integer vector, Niveau maximum recommandé pour s'aventurer dans la zone}
#'   \item{\code{Min_bot_level}}{Integer vector, Niveau minimum pour pouvoir utiliser un bot dans la zone}
#'   \item{\code{Max_bot_level}}{Integer vector, Niveau maximum pour pouvoir utiliser un bot dans la zone} 
#'}
#' @details Ce jeu de données a été extrait de kaggle où il est possible de trouver une description complémentaire des variables.
#' @source Kaggle. 
"d_wow"
#> [1] "d_wow"

#' @title Matchs de football joués en France de 2011/2012 à 2016/2017
#' 
#' @description Ce fichier de données fournit une vue granulaire de 208 446 matchs de football joués en France depuis la saison 2011/2012 à la saison 2016/2017. 
#' 
#' @format Un objet \code{data.frame} avec 208446 lignes et 40 variables :
#' \describe{
#'   \item{\code{X}}{Integer vector, identifiant unique de ligne}
#'   \item{\code{id_odsp}}{Factor w/ 2076 levels, identifiant unique de match}
#'   \item{\code{id_event}}{Factor w/ 208446, unique identifier of event (id_odsp + sort_order)}
#'   \item{\code{sort_order}}{Integer vector, chronological sequence of events in a game}
#'   \item{\code{time}}{Integer vector, minute of the game}
#'   \item{\code{text}}{Factor w/ 79629 levels, text commentary}
#'   \item{\code{event_type}}{Integer vector, primary event. 11 unique events (1-Attempt(shot), 2-Corner, 3-Foul, 4-Yellow Card, 5-Second yellow card, 6-(Straight) red card, 7-Substitution, 8-Free kick won, 9-Offside, 10-Hand Ball, 11-Penalty conceded)}
#'   \item{\code{event_type2}}{Integer vector, secondary event. 4 unique events (12 - Key Pass, 13 - Failed through ball, 14-Sending off, 15-Own goal)}
#'   \item{\code{side}}{Integer vector, 1-Home, 2-Away}
#'   \item{\code{event_team}}{Factor w/ 30 levels, Équipe de football qui est à l’origine de l’événement. In case of Own goals, event team is the team that beneficiated from the own goal}
#'   \item{\code{opponent}}{Factor w/ 30 levels, team that the event happened against}
#'   \item{\code{player}}{Factor w/ 1609, name of the player involved in main event (converted to lowercase and special chars were removed)}
#'   \item{\code{player2}}{Factor w/ 1498, name of player involved in secondary event}
#'   \item{\code{player_in}}{Factor w/ 1277, player that came in (only applies to substitutions)}
#'   \item{\code{player_out}}{Factor w/ 1204, player substituted (only applies to substitutions)}
#'   \item{\code{shot_place}}{Integer vector, placement of the shot (13 possible placement locations, available in the dictionary, only applies to shots)}
#'   \item{\code{shot_outcome}}{Integer vector, 4 possible outcomes (1-On target, 2-Off target, 3-Blocked, 4-Hit the post)}
#'   \item{\code{is_goal}}{Integer vector, binary variable if the shot resulted in a goal (own goals included)}
#'   \item{\code{location}}{Integer vector, location on the pitch where the event happened (19 possible locations, available in the dictionary)}
#'   \item{\code{bodypart}}{Integer vector, (1- right foot, 2-left foot, 3-head)}
#'   \item{\code{assist_method}}{Integer vector, in case of an assisted shot, 5 possible assist methods (details in the dictionary)}
#'   \item{\code{situation}}{Integer vector, 4 types: 1-Open Play, 2-Set piece (excluding Direct Free kicks), 3-Corner, 4-Free kick}
#'   \item{\code{fast_break}}{Integer vector, binary}
#'   \item{\code{link_odsp}}{Factor w/ 2076 levels lien vers la page oddsportal}
#'   \item{\code{adv_stats}}{Logical vector, boolean if the game has detailed event data}
#'   \item{\code{date}}{Factor w/ 592 levels, Date of game}
#'   \item{\code{league}}{Factor w/ 1 level, Club League}
#'   \item{\code{season}}{Integer vector, Year Played}
#'   \item{\code{country}}{Factor w/ 1 level, Host Nation of League}
#'   \item{\code{ht}}{Factor w/ 30 levels, home team}
#'   \item{\code{at}}{Factor w/ 30 levels, away team}
#'   \item{\code{fthg}}{Integer vector, full time home goals}
#'   \item{\code{ftag}}{Integer vector, full time away goals}
#'   \item{\code{odd_h}}{Numerical vector, highest home win market odds}
#'   \item{\code{odd_d}}{Numerical vector, highest draw market odds}
#'   \item{\code{odd_a}}{Numerical vector, highest away market odds}
#'   \item{\code{odd_over}}{Numerical vector, highest over 2.5 market odds}
#'   \item{\code{odd_under}}{Numerical vector, highest under 2.5 market odds}
#'   \item{\code{odd_bts}}{Numerical vector, highest both teams to score market odds}
#'   \item{\code{odd_bts_n}}{Numerical vector, highest both teams NOT to score market odds} 
#'}
#' @details Ces données sont une version « nettoyée » d’un fichier original, events_France.csv, qu’il est possible de télécharger depuis la plate-forme Kaggle : \url{https://www.kaggle.com/secareanualin/football-events}.
#' Certains matchs contiennent cependant des données manquantes (environ 10%). 
#' @source Kaggle. 
"data_event"
#> [1] "data_event"

#' @title Jeu de données école (2 variables)
#' 
#' @description Jeu de données école avec les résultats des tests de mathématiques et de sport. Ces données ont été simulées.
#' 
#' @format Un objet \code{data.frame} avec 119 lignes et 2 variables :
#' \describe{
#'   \item{\code{Maths}}{Numerical vector, Note en mathématiques}
#'   \item{\code{Sport}}{Numerical vector, Note en sport} 
#'}
"ecole2"
#> [1] "ecole2"

#' @title Jeu de données école (3 variables)
#' 
#' @description Jeu de données école avec non seulement les résultats des tests de mathématiques et de sport mais aussi l'âge des élèves. Ces données ont été simulées.
#' 
#' @format Un objet \code{data.frame} avec 119 lignes et 3 variables :
#' \describe{
#'   \item{\code{Maths}}{Numerical vector, Note en mathématiques}
#'   \item{\code{Sport}}{Numerical vector, Note en sport}
#'   \item{\code{Age}}{Numerical vector, Âge de l'élève} 
#'}
"ecole3"
#> [1] "ecole3"

#' @title Parasites
#' 
#' @description Jeu de données sur léthalité de la cypermethrine sur des parasites.
#' 
#' @format Un objet \code{data.frame} avec 12 lignes et 4 variables :
#' \describe{
#'   \item{\code{Total}}{Integer vector, Nombre total de parasites}
#'   \item{\code{N.morts}}{Integer vector, Nombre de parasites morts}
#'   \item{\code{Niveau.de.dose}}{Integer vector, Niveau de dose utilisé}
#'   \item{\code{Sexe}}{Factor w/ 2 levels, Sexe de l'hôte} 
#'}
"parasites"
#> [1] "parasites"

#' @title Pokemons
#' 
#' @description Jeu de données contenant les caractéristiques de pokemons. Les pokemons concernés apparaissent dans les jeux et non dans les cartes pokemon ou Pokemon Go..
#' 
#' @format Un objet \code{data.frame} avec 800 lignes et 13 variables :
#' \describe{
#'   \item{\code{X.}}{Integer vector, PokeDex index number}
#'   \item{\code{Name}}{Factor w/ 800 levels, Name of the Pokemon}
#'   \item{\code{Type.1}}{Factor w/ 18 levels, Type of pokemon}
#'   \item{\code{Type.2}}{Factor w/ 18 levels, Other Type of Pokemon}
#'   \item{\code{Total}}{Integer vector, Sum of Attack, Sp. Atk, Defense, Sp. Def, Speed and HP}
#'   \item{\code{HP}}{Integer vector, Hit Points}
#'   \item{\code{Attack}}{Integer vector, Attack Strength}
#'   \item{\code{Defense}}{Integer vector, Defensive Strength}
#'   \item{\code{Sp..Atk}}{Integer vector, Special Attack Strength}
#'   \item{\code{Sp..Def}}{Integer vector, Special Defensive Strength}
#'   \item{\code{Speed}}{ Speed}
#'   \item{\code{Generation}}{Integer vector, Number of generation}
#'   \item{\code{Legendary}}{Factor w/ 2 levels, True if Legendary Pokemon False if not (more revision on mythical vs legendary needed)} 
#'}
#' @details Ce jeu de données a été extrait de kaggle où il est possible de trouver une description complémentaire des variables.
#' @source Kaggle. 
"poke"
#> [1] "poke"

#' @title Polypes
#' 
#' @description Nombre de polypes chez des sujets lors d'un essai clinique.
#' 
#' @format Un objet \code{data.frame} avec 20 lignes et 3 variables :
#' \describe{
#'   \item{\code{nombre}}{Integer vector, Nombre de polypes après 12 mois}
#'   \item{\code{traitement}}{Factor w/ 2 levels, Bras de l'essai, un facteur avec deux niveaux placebo et medicament}
#'   \item{\code{age}}{Integer vector, Âge du patient} 
#'}
#' @source M. Giardiello, S. R. Hamilton, A. J. Krush, S. Piantadosi, L. M. Hylind, P. Celano, S. V. Booker, C. R. Robinson and G. J. A. Offerhaus (1993), Treatment of colonic and rectal adenomas with sulindac in familial adenomatous polyposis. New England Journal of Medicine, 328(18), 1313–1316.
#' S. Piantadosi (1997), Clinical Trials: A Methodologic Perspective. John Wiley and Sons, New York.
"polypes"
#> [1] "polypes"

#' @title Résistance d'un ciment
#' 
#' @description Jeu de données contenant les résultats d'une expérience évaluant la résistance d'un ciment.
#' 
#' @format Un objet \code{data.frame} avec 36 lignes et 3 variables :
#' \describe{
#'   \item{\code{Melangeur}}{Factor w/ 3, COLUMN_DESCRIPTION}
#'   \item{\code{Casseur}}{Factor w/ 3, COLUMN_DESCRIPTION}
#'   \item{\code{Resistance}}{Integer vector, résistance en livres par pouces carrés} 
#'}
#' @details Davies et Goldsmith ont récolté les données d’une expérience dont le but était d’étudier les différentes sources de variabilité possibles de la résistance d’un ciment fabriqué à Portland. 
#' L’expérience s’est déroulée ainsi : plusieurs petits prélèvements d’un même type de ciment ont été mélangés à de l’eau et travaillés par trois personnes différentes, les « mélangeurs ». On a alors formé douze cubes à l’aide de chacune des préparations des « mélangeurs ». 
#' Puis on a donné ces 36 cubes à trois personnes chargées d’évaluer leur résistance, les « casseurs ». 
#' La répartition des 36 cubes entre ces « casseurs » a été faite de telle sorte que chaque « casseur » reçoive quatre cubes provenant de chacune des préparations des « mélangeurs » soit douze cubes au total.
#' Tous les tests de résistance ont été faits sur la même machine. L’objectif principal de cette expérience était d’étudier et de quantifier l’importance de la variabilité dans les tests de résistance qui pouvait provenir des différences individuelles entre les « mélangeurs » et les « casseurs ». 
#' Les données ci-dessous, exprimées dans les unités d’origine c’est-à-dire .
#' @source Davies, O.L. et Goldsmith, P.L. (Eds.), \emph{Statistical Methods in Research and Production}, 4th edition, Oliver and Boyd, Edinburgh, 1972.
"resistance"
#> [1] "resistance"

#' @title Sida du chat
#' @description Taux de leucocytes
#' @format Un objet \code{data.frame} avec 33 lignes et 3 variables :
#' \describe{
#'   \item{\code{Sexe}}{Factor w/ 2 levels, Sexe de l'animal}
#'   \item{\code{Jours}}{Integer vector, Nombre de jours après l'inoculation}
#'   \item{\code{LnT4}}{Numerical vector, Logarithme népérien du taux de leucocytes T4} 
#'}
#' @details Le taux de leucocytes T4 chez le chat a été mesuré plusieurs jours (valeur de la variable Jours) après avoir inoculé à l’animal le virus FeLV, analogue au HIV. 
#' Nous appellons LnT4 le logarithme népérien de ce taux de leucocytes T4.
#' @source \emph{Modèle linéaire : Comparaison de groupes et régression} de B. Prum aux Éditions de l’INSERM, 1996.
"SidaChat"
#> [1] "SidaChat"

#' @title Vitamines
#' @description Influence de différents régimes alimentaires sur des rats de laboratoire.
#' @format Un objet \code{data.frame} avec 32 lignes et 3 variables :
#' \describe{
#'   \item{\code{Calorie}}{Integer vector, COLUMN_DESCRIPTION}
#'   \item{\code{Vitamine}}{Integer vector, COLUMN_DESCRIPTION}
#'   \item{\code{Poids}}{Integer vector, COLUMN_DESCRIPTION} 
#'}
#' @details Le gain de poids des rats est désigné par la variable Poids, exprimée en grammes, les deux facteurs sont les variables Calorie et Vitamine. 
#' La variable Calorie vaut 1 si les rats n’ont pas suivi un régime hypercalorique et 2 s’ils ont suivi un tel régime hypercalorique. 
#' La variable Vitamine vaut 1 si les rats n’ont pas reçu de compléments vitaminés et 2 s’ils ont reçu de tels compléments.
#' @source D’après B. Falissard. \emph{Comprendre et utiliser les statistiques dans les sciences de la vie}. Masson, 2005.
"vitamines"
#> [1] "vitamines"

