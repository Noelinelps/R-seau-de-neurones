options(spinner.color="navy")
#setwd("D:/Documents/FAC/M2/semestre 10/- Projets/reseau de neurones/FINAL")


header=dashboardHeader(
  title="Menu",
  titleWidth = 200,
  tags$li(a(strong("Master ESA"),
            href = "https://www.master-esa.fr"),
          class = "dropdown")
)

sidebar=dashboardSidebar( 
  width = 200,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    menuItem( "Présentation", tabName = 'pres', icon = icon('book')),
    
    menuItem("Données", tabName = "dat", icon = icon('chart-bar'),
             menuSubItem('Crédit', tabName="stat", icon=icon("circle")),
             menuSubItem('Kaggle', tabName="stats", icon=icon("circle"))),
    
    menuItem("Explication", tabName="neural", icon=icon("edit"),
             menuSubItem('PMC', tabName="pmc", icon=icon("circle")),
             menuSubItem('RBF', tabName="rbf", icon=icon("circle"))
    ),
    
    menuItem('Application', tabName = "modele", icon = icon('layer-group'),
             menuSubItem('Régression logistique', tabName = "reg", icon = icon('circle')),
             menuSubItem('PMC', tabName = "pmc2", icon = icon('circle')),
             menuSubItem('RBF', tabName = "rbf2", icon = icon('circle')),
             menuSubItem('Arbre de décision', tabName = "arbres", icon = icon('circle')),
             menuSubItem('Forêts aléatoires', tabName = "rforest", icon = icon('circle')),
             menuSubItem('Gradient boosting', tabName = "boost", icon = icon('circle')),
             menuSubItem('SVM', tabName = "svm", icon = icon('circle'))
    ),
    
    menuItem('Comparaison', tabName = "compare", icon = icon("not-equal")),
    
    menuItem( "À propros", tabName = 'propos', icon = icon('user'))
  ))




body= dashboardBody(
  tabItems(
    tabItem(tabName="pres",
            img(src='image.png', width = "1100px",style="display: block; margin-left: auto; margin-right: auto;"),
            br(),
            br(),
            h2("Présentation", align="center"),
            br(),
            br(),
            p(style="text-align: justify;", "Le but de ce travail est de se focaliser sur un problème de classification. Plus précisement, sur la probabilité de défaut.",
              "Dans ce contexte, nous mettons en compétition plusieurs algorithmes et méthodes (Régression logistique, Arbres de décisions, Forêts aléatoires, Gradient
              Boosting, SVM et enfin 2 types de Réseaux de Neurones : le PMC et le RBF) dans le but de sélectionner celle qui présentera le plus fort pouvoir de généralisation.",
              br(),
              br(),
              "Pour cela, nous allons appliquer ces modèles dans un premier temps d'une manière générale sur un jeu de données nommé 'Crédit' et ensuite d'une manière plus précise, 
              les appliqué à un jeu de données Kaggle : 'Give me some credit'. Le but ici est de prédire la probabilité de défaut qui survient lorsqu'un individu passe au moins 90 jours 
              en défaut sur la période étudiée.",
              br(),br(),
              "Dans un premier temps, afin d'avoir une idée de nos données, nous étudions quelques statistiques descriptives ainsi que la répartition des variables 
              (cible et explicatives).",
              br(),br(),
              "Dans un deuxième temps, nous modélisons les algorithmes et méthodes énoncées sur nos jeux de données et prenons en compte la matrice de confusion, l’aire 
              sous la courbe ROC (AUC) et la précision de chaque modèle sur l’échantillon test.", 
              br(), br(),
              "Enfin, la dernière partie de notre travail consiste à comparer chaque méthode. Nous comparons donc l'AUC et la précision des modèles afin de sélectionner le
              meilleur. Puis nous comparons également l'AUC et la précison de l'échantillon d'apprentissage à celles de l'échantillon test afin de détecter un éventuel sur-apprentissage.")
    ),
    
    
    tabItem(tabName="stat", 
            h2("Statistiques descriptives: Crédit", align="center"),
            br(),
            br(),
            p(style="text-align: justify;", "Observer les données brutes est une étape primordiale avant de passer à la modélisation des donneés. Pour cela, affichons le début de notre jeu de données."),
            br(),
            box( width=12, title="Affichage des premières observations", withSpinner(tableOutput("tab1")),status="navy", solidHeader=T,style = "height:500;overflow-x: scroll;"),
            br(),
            p(style="text-align: justify;", "Afin de mieux comprendre le jeu de données, vous pouvez télécharger un PDF explicant les différentes variables."),
            actionButton("pdf","Description des variables", icon=icon("mouse-pointer"), style="float:right; color: #fff; background-color: #000035; border-color: #000035"),
            br(),
            br(),
            br(),
            p(style="text-align: justify;", "Nos données ne comportaient pas de valeurs manquantes mais quelques doublons ainsi que des problèmes de modalités."), 
            br(),
            br(),
            
            hr(style="border-top: 1px solid #000000;"),
            h3("Statistiques descriptives classiques", align="left"),
            br(),
            withSpinner(verbatimTextOutput("summary1")),align="center",
            br(),
            p(align="justify",style="text_align:justify;", "À première vue, il ne semble pas y avoir de problème dans notre jeu de données. Notre variable cible est bien binaire, ils ne semblent pas y avoir de valeurs aberrantes."),
            br(),
            hr(style="border-top: 1px solid #000000;"),
            h3("Répartition de la variable cible", align="left"),
            br(),
            fluidRow(
              withSpinner(plotOutput("gra"))),
            br(),
            p(align="justify",style="text_align:justify","Le pourcentage de défaut n'est pas trop faible, nous avons une bonne répartition de notre variable cible."),
            br(),
            br(),
            
            h3("Répartition des variables explicatives", align="left"),
            br(),
            withSpinner(plotOutput("gra1")),
            br(),
            h5("Historique des paiements", align="left"),
            withSpinner(plotOutput("gra2")),
            br(),
            h5("Montant des factures", align="left"),
            withSpinner(plotOutput("gra3")),
            br(),
            h5("Montant des anciens paiements", align="left"),
            withSpinner(plotOutput("gra4")),
            br(),
            p(style="text_align:justify", align="justify", "La répartition des variables explicatives n'indique pas de problème. Nous pouvons maintenant standardiser les données car c'est une étape nécessaire pour certaine méthode : celle des réseaux de neurones.")),
    
    
    tabItem(tabName="stats", 
            h2("Statistiques descriptives: Kaggle", align="center"),
            br(),
            br(),
            p(style="text-align: justify;", "Nous affichons ici aussi le début de notre jeu de données afin de pouvoir observer les données brutes avant de passer à la modélisation."),
            br(),
            box( width=12, title="Affichage des premières observations", withSpinner(tableOutput("tab")),status="navy", solidHeader=T,style = "height:500;overflow-x: scroll;"),
            br(),
            p(style="text-align: justify;", "Afin de mieux comprendre le jeu de données, vous pouvez télécharger un PDF explicant les différentes variables."),
            actionButton("pdf1", "Description des variables", icon=icon("mouse-pointer"), style="float:right; color: #fff; background-color: #000035; border-color: #000035"),
            br(),
            br(),
            br(),
            p(style="text-align: justify;", "Nos données comportaient quelques valeurs manquantes sur deux variables quantitatives. Celles ci ont donc été imputées par la médiane. Cette décision se base sur la proportion faible de valeurs manquantes mais également par le fait que la médiane n'est pas affectée par les outliers contraitrement à la moyenne."), 
            br(),
            br(),
            hr(style="border-top: 1px solid #000000;"),
            h3("Statistiques descriptives classiques", align="left"),
            br(),
            withSpinner(verbatimTextOutput("summary")),align="center",
            br(),
            p(align="justify",style="text_align:justify;", "À première vue, il ne semble pas y avoir de problème dans notre jeu de données, notre variable cible est bien binaire, toutes les variables explicatives sont quantitatives, les minimums et maximums des variables ne semblent pas indiquer de valeurs aberrantes."),
            br(),
            hr(style="border-top: 1px solid #000000;"),
            h3("Répartition de la variable cible", align="left"),
            br(),
            fluidRow(
              withSpinner(plotOutput("graph1"))),
            br(),
            p(align="justify",style="text_align:justify","Le pourcentage de défaut est très faible : nous sommes en présence d'un évènement rare. Cela risque de poser problème lors de notre modélisation, nous devons donc effectuer un rééchantillonnage de nos données afin d'augmenter le pourcentage de défaut. Nous appliquons un rééchantillonnage de type SMOTE qui va d'une part, augmenter le nombre d'événement en dupliquant certains individus ayant fait défaut et d'une autre part, réduire les non-événements en supprimant quelques individus n’ayant pas fait défaut.", "Après rééchantillonnage, nous obtenons la répartition de droite. Le rééchantillonnage va poser problème lors de la construction des matrices de confusion qu'il faudra corriger."),
            br(),
            br(),
            h3("Répartition des variables explicatives", align="left"),
            br(),
            withSpinner(plotOutput("graph3")),
            br(),
            p(style="text_align:justify", align="justify", "Nous avons standardisé les données afin que cela ne pose aucun problème pour la modélisation et nous voyons que la répartition des variables explicatives n'indique effectivement pas de problème. ")),
    
    tabItem(tabName = "pmc",
            h2("Réseau de neurones - Perceptron MultiCouche (PMC)", align="center"),
            br(),
            p(style="text-align: justify;",
              "Un réseau de neurones est appelé ainsi car c'est une méthode de machine learning qui s'inspire du cerveau humain avec un ensemble de cellules connectées capables de créer, envoyer et recevoir des signaux.
              Il existe plusieurs types de réseaux de neurones, intéresserons nous au PMC (Perceptron MultiCouche).", br(),
              "Le PMC peut avoir plusieurs couches intermédiaires, la première couche correspond aux entrées et la dernière aux sorties. Les couches intermédiaires successives recoivent chacune des signaux de la couche précédente et partagent à
              la couche suivante de nouveaux signaux. De part son architecture simple, c'est le plus populaire.",br(),
              "Il existe des fonctions d'activation qui sont parfois appliquées à chaque entrée/sortie qui peuvent être linéaires ou non. Elles sont définies en fonction de la variable cible car elles ont un impact 
              sur la prévision. La fonction d'activation finale est appelée fonction de transfert.",br(),
              "Pour le PMC, une fois que les fonctions d'activations adaptées sont déterminées, le réseau de neurone apprend à modéliser les données grâce à l'ensemble d'apprentissage. Nous allons ainsi ajuster les paramètres (poids,biais)
              afin que le réseau de neurones fournisse les meilleurs prédictions. Le but est de minimiser notre fonction de perte. L'optimisation peut être résolue par apprentissage adaptatif ou non adaptatif.",br(),br(),br(),
              "Vous disposez de 3 scénarios possibles, tous ont un nombre de couches intermédiaires différents. Attention cependant, cette méthode est parfois longue à implémenter."),
            br(),
            tabBox(
              title = "Choix", width = 14,
              selectInput("scenario",
                          label="Scénarios possibles",choices=list("1"=1,"2"=2,"3"=3),selected=1)),
            br(),
            h5("Architecture du réseau", align="left"),
            withSpinner(plotOutput("graph_pmc")),
            br(),
            box(title="Matrice de confusion", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfpmc_credit")),align="center",style = "height:500;"),
            valueBoxOutput("accpmc_credit"),
            valueBoxOutput("aucpmc_credit")
    ),
    
    
    tabItem(tabName = "rbf",
            h2("Réseau de neurones - Radial Basis Function (RBF)", align="center"),
            br(),
            p(style="text-align: justify;",
              "Un réseau de neurones est appelé ainsi car c'est une méthode de machine learning qui s'inspire du cerveau humain avec un ensemble de cellules connectées capables de créer, envoyer et recevoir des signaux.
              Il existe plusieurs types de réseaux de neurones, intéresserons nous au RBF (Radial Basis Function).", br(), "Le réseau de neurones RBF a pour objectif de pallier certaines limites du PMC. nous allons donc réduire la complexité d'apprentissage d'un PMC (à la fois par la dimension du vecteur de paramètre à ajuster
               et par la quantité prohibitive de la fonction de perte à minimiser). Un RBF est constitué d'une seule couche intermédiaire et agit localement dans l'espace des entrées, de plus il n'y a pas de biais.",br(),
              "Les fonctions de transferts sont des fonctions à bases radiales (comme par exemple la famille des exponentielles.)"),
            br(),
            tabBox(
              title = "Choix", width = 14,
              numericInput("nh", "Nombre de neurones cachés",2),
              tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000033}")),
              sliderInput("cutoff1","Cut off", width=1400, min = 0, max = 1, value = 0.3, step = 0.1)),
            br(),
            box(title="Matrice de confusion", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfrbf_credit")),align="center",style = "height:500;"),
            valueBoxOutput("accrbf_credit"),
            valueBoxOutput("aucrbf_credit")
    ),
    
    
    tabItem(tabName = "reg",
            h2("Explication Régression logistique", align="center"),
            br(),
            p(style="text-align: justify;",
              "La régression logistique est un modèle qui permet d'étudier les relations entre des variables explicatives qualitatives ou quantitatives, noté X, avec une variable à expliquer qualitiatve noté Y. Il s'agit d'un modèle linéaire généralisé avec une fonction logistique comme fonction de lien.
              Ce modèle peut aussi permettre de prédire la probabilité de survenue d'un évènement (Y=0 ou Y=1)."),
            br(),
            
            tabBox(
              title = "Choix", width = 14,
              tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000033}")),
              sliderInput("cutoff2","Cut off", width=1400, min = 0, max = 1, value = 0.4, step = 0.1)),
            br(),
            box(title="Matrice de confusion", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfreg_kaggle")),align="center",style = "height:700;"),
            valueBoxOutput("accreg_kaggle"),
            valueBoxOutput("aucreg_kaggle")
    ),
    
    
    tabItem(tabName = "pmc2",
            h2("Réseau de neurones - Perceptron MultiCouche (PMC)", align="center"),
            br(),
            p(style="text-align: justify;",
              "Un réseau de neurones est appelé ainsi car c'est une méthode de machine learning qui s'inspire du cerveau humain avec un ensemble de cellules connectées capables de créer, envoyer et recevoir des signaux.
              Il existe plusieurs types de réseaux de neurones, intéresserons nous au PMC (Perceptron MultiCouche).", br(),
              "Le PMC peut avoir plusieurs couches intermédiaires, la première couche correspond aux entrées et la dernière aux sorties. Les couches intermédiaires successives recoivent chacune des signaux de la couche précédente et partagent à
              la couche suivante de nouveaux signaux. De part son architecture simple, c'est le plus populaire.",br(),
              "Il existe des fonctions d'activation qui sont parfois appliquées à chaque entrée/sortie qui peuvent être linéaires ou non. Elles sont définies en fonction de la variable cible car elles ont un impact 
              sur la prévision. La fonction d'activation finale est appelée fonction de transfert.",br(),
              "Pour le PMC, une fois que les fonctions d'activations adaptées sont déterminées, le réseau de neurone apprend à modéliser les données grâce à l'ensemble d'apprentissage. Nous allons ainsi ajuster les paramètres (poids,biais)
              afin que le réseau de neurones fournisse les meilleurs prédictions. Le but est de minimiser notre fonction de perte. L'optimisation peut être résolue par apprentissage adaptatif ou non adaptatif.",br(),br(),br(),
              "Vous disposez de 3 scénarios possibles, tous ont un nombre de couches intermédiaires différents. Attention cependant, cette méthode est parfois longue à implémenter."), br(), br(),
            p(style="text-align: justify;","Malheureusement, l'une des limites de l'utilisation du logiciel R se retrouve atteinte. Au vue de la taille du jeu de données et de la complexité d'un réseau de neurones les algorithmes ne parviennent pas à converger.")
    ),
    
    
    tabItem(tabName = "rbf2",
            h2("Réseau de neurones - Radial Basis Function (RBF)", align="center"),
            br(),
            p(style="text-align: justify;",
              "Un réseau de neurones est appelé ainsi car c'est une méthode de machine learning qui s'inspire du cerveau humain avec un ensemble de cellules connectées capables de créer, envoyer et recevoir des signaux.
              Il existe plusieurs types de réseaux de neurones, intéresserons nous au RBF (Radial Basis Function).", br(), "Le réseau de neurones RBF a pour objectif de pallier certaines limites du PMC. nous allons donc réduire la complexité d'apprentissage d'un PMC (à la fois par la dimension du vecteur de paramètre à ajuster
               et par la quantité prohibitive de la fonction de perte à minimiser). Un RBF est constitué d'une seule couche intermédiaire et agit localement dans l'espace des entrées, de plus il n'y a pas de biais.",br(),
              "Les fonctions de transferts sont des fonctions à bases radiales (comme par exemple la famille des exponentielles.)"),
            br(),
            tabBox(
              title = "Choix", width = 14,
              numericInput("nh2", "Nombre de neurones cachés",2),
              tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #000033}")),
              sliderInput("cutoff4","Cut off", width=1400, min = 0, max = 1, value = 0.3, step = 0.1)),
            br(),
            box(title="Matrice de confusion", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfrbf_kaggle")),align="center",style = "height:500;"),
            valueBoxOutput("accrbf_kaggle"),
            valueBoxOutput("aucrbf_kaggle")
    ),
    
    
    tabItem(tabName = "arbres",
            h2("Arbre de décision", align="center"),
            br(),
            withMathJax(),
            p(style="text-align: justify;",
              "Un arbre de décision est un outil d'aide à la décision (comme son nom l'indique). C'est un schéma sous forme d'arbre qui commence par un noeud, appelé racine, d'où découlent plusieurs résultats possibles. Ces résultats mènent à d'autres nœuds, d'où émanent d'autres possibilités.
              Chaque noeud est découpé par une variable, appelé variable de césure, c'est la variable qui réalise le meilleur partage de l'ensemble. Les noeuds terminaux sont appelés les feuilles de l'arbre. La modalité prédite par l'arbre dans le cas d'une classification correspond à la modalité la plus prise. Un paramétre de complexité va permettre de jouer sur la profondeur de l'arbre et donc sur sa capacité de prédiction."),
            br(),
            tabBox(
              title = "Choix", width = 14,
              tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #000033}")),
              sliderInput("cp","Paramétre de complexité", width=1400, min = 0.0001, max = 0.01, value = 0.01, step = 0.0001)),
            br(),
            p("NB: L'arbre peut devenir illisible si le paramétre de complexité est très faible."),
            withSpinner(plotOutput("graph_arbre")),
            box(title="Matrice de confusion", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfarbre_kaggle")),align="center",style = "height:500;"),
            valueBoxOutput("accarbre_kaggle"),
            valueBoxOutput("aucarbre_kaggle")
    ),
    
    
    tabItem(tabName = "rforest",
            h2("Random Forest", align="center"),
            br(),
            p(style="text-align: justify;",
              "Cet algorithme appartient à la famille des agrégations de modèles, c'est en fait un cas particulier du Bagging, appliqué aux arbres de décision de type CART.",br(),
              "Le principe des forêts aléatoires, est de faire la moyenne des prévisions de plusieurs arbres de décision indépendants pour réduire 
            la variance et donc l'erreur de prévision.",br(),"Pour construire ces différents arbres, nous sélectionnons plusieurs échantillons bootstrapés, c'est à dire des tirages avec remise dans les individus.
            A cela s'ajoute un sous-ensemble de variables tirées aléatoirement, souvent égal à \\(\\sqrt{p}\\) prédicteurs.",br(),
              "Nous nous retrouvons donc avec plusieurs arbres de décision et donc des prédictions différentes pour chaque individu. L'estimation finale se fait :",br(),
              "- Dans le cas d'une classification : nous choisissons la catégorie la plus fréquente (le mode)",br(),
              "- Dans le cas d'une régression : nous faisons la moyenne des valeurs prédites.",br(),
             ),
            br(),
            p(style="text-align: justify;", "NB: Prendre un nombre élévé d'arbres augmente considérablement le temps d'affichage."),
            
            tabBox(
              tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #000033}")),
              title = "Choix", width = 14,
              sliderInput("ntree","Nombre d'arbres", width=1400,min = 0, max = 200, value = 10, step = 5)),
            br(),
            box(title="Matrice de confusion corrigée", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfurf")),align="center",style = "height:500;"),
            valueBoxOutput("acc.rf"),
            valueBoxOutput("auc.rf")),
    
    
    tabItem(tabName = "boost",
            h2("Boosting", align="center"),
            br(),
            p(style="text-align: justify;",
              "L'idée du Boosting est d'utiliser plusieurs hypothèses faibles et consiste en la réalisation d'itérations.",br(), 
              "Dans  la construction des modèles, le Boosting travaille de manière séquentielle : il commence par construire un premier modèle qu'il va évaluer. 
              Puis, à partir de cette mesure, chaque individu va être pondéré en fonction de la performance de la prédiction.",br(),
              "L'objectif est de donner un poids plus important aux individus pour lesquels la valeur a été mal prédite pour la construction du modèle suivant. 
              Le fait de corriger les poids au fur et à mesure permet de mieux prédire les valeurs difficiles."),
            br(),
            p(style="text-align: justify;", "NB: Prendre un nombre élévé d'itérations augmente considérablement le temps d'affichage."),
            
            tabBox(
              tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #000033}")),
              title = "Choix", width = 14,
              sliderInput("mfinal","Nombre d'itérations", width=1400, min = 0, max = 200, value = 10, step = 5)),
            br(),
            box(title="Matrice de confusion corrigée", status="navy", solidHeader=T, withSpinner(tableOutput("matconfuboost")),align="center",style = "height:500;"),
            valueBoxOutput("acc.boost"),
            valueBoxOutput("auc.boost")),
    
    
    tabItem(tabName = "svm",
            h2("Explication svm", align="center"),
            br(),
            
            withMathJax(),
            p(style="text-align: justify;",
              "Le SVM : 'Support Vector Machine', ou 'Séparateurs à Vastes Marges' est une méthode de machine learning supervisée permettant la classification des individus. Le principe est que les données
              sont linéairement séparables. Nous pouvons donc séparer les individus en 2 sous-groupes, chaque groupe correspondant à une prédiction. Dans le cas où les données ne sont pas linéairement séparables, 
              l'utilisation d'une astuce est alors requise. L'astuce Kernel permet de projeter les individus dans un espace de plus grande dimension afin d'avoir cette fois des individus linéairement séparables.",
              br(),"C'est une méthode simple à utiliser mais difficile à interpréter : c'est une boîte noire."),
            br(),
            box(title="Matrice de confusion", status="navy", solidHeader=T ,withSpinner(tableOutput("matconfsvm_kaggle")),align="center",style = "height:700;"),
            valueBoxOutput("accsvm_kaggle"),
            valueBoxOutput("aucsvm_kaggle")
    ),
    
    
    tabItem(tabName="compare",
            h2("Comparaison des méthodes", align="center"),
            br(),
            br(),
            p(style="text-align: justify;",
              "Nous pouvons à présent comparer chaque méthode afin de choisir laquelle convient le mieux à notre jeu de données: Kaggle.",br(),
              "Pour comparer au mieux les modèles, nous avons déterminé les paramètres optimaux de chacune des méthodes par validation croisée (pour des raisons de gain de temps le random Forest sera limité à 10 arbres et le boosting à 10 itérations).",br(),br(),
              "Nous comparons les AUC ainsi que les précisions de chaque modèle sur les échantillons tests. Cependant, nous prêtons tout de même attention à celles des échantillons d'apprentissage afin de détecter un possible sur-apprentissage."),
            br(),
            br(),
            box(width=12, title="Comparaison", status="navy", solidHeader=T,withSpinner(tableOutput("comparaison2")),align="center",style = "height:500;"),
            br(),
            p(style="text-align: justify;",
              "Au vue de ces résultats, le meilleur modèle est le Random Forest. Cependant l'écart important entre l'AUC et la précision de l'échantillon test avec ceux de l'échantillon train indique un sur-apprentissage. Le meilleur modèle qui ne semble pas soumis au sur-apprentissage est le Boosting.",br(),br(),
              "En ce qui concerne les réseaux de neurones, nous n'avons pu mettre en place sur les données Kaggle uniquement un RBF. Celui-ci est meilleur que la régression logistique mais pas que certaines autres méthodes. L'impossibilité de mettre en place un PMC ne permet pas de comparer le pouvoir prédictif de ce modèle.")),
    
    
    tabItem(tabName = "propos",
            h2("Notre Equipe",align="center"),
            br(),
            br(),
            p(style="text-align: justify;",
              "Cette application a été réalisée dans le cadre d'un projet de la formation Master économétrie et statistique appliquée de l'Université d'Orléans par deux étudiantes de deuxième année.", br(),br(),
              "Ce projet est lié au cours Big Data Neural - Networks (enseigné par M. NDOYE)."),
            br(),
            br(),
            
            hr(style="border-top: 1px solid #000000;"),
            br(),
            p(style="text-align: justify;", "Pour toute question concernant cette application, n'hésitez pas à nous contacter."),
            br(),
            fluidRow(userBox(
              title = userDescription(
                title =tags$a(href='https://www.linkedin.com/in/no%C3%ABline-lepais-731040174/',
                              icon("linkedin"),
                              'Noeline LEPAIS'),
                type = 1,
                image = "https://media-exp1.licdn.com/dms/image/C4E03AQFoDhRQd8jexw/profile-displayphoto-shrink_800_800/0/1639325629962?e=1648080000&v=beta&t=8dsZaoa-qOEiR0welua5_Olo2cPdjsblUrDAW4CDh_w")
            ),
            userBox(
              title = userDescription(
                title =tags$a(href='https://www.linkedin.com/in/lou-daccord-ab668b1a3/',
                              icon("linkedin"),
                              'Lou DACCORD'),
                type = 1,
                image = "https://media-exp1.licdn.com/dms/image/C4D03AQGHt9VVHK6AOA/profile-displayphoto-shrink_800_800/0/1633191506540?e=1648080000&v=beta&t=995PS3m9PsPTtkz6gPhKViNPmEduNO4L1ntGO2fWaWk",
              ))),
            br(),
            hr(style="border-top: 1px solid #000000;"))
  )
)


ui=dashboardPage(header, sidebar, body, 
                 skin = "blue" )



