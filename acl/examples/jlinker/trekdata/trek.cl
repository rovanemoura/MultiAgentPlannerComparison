
;; $Id: trek.cl,v 5.0 2004/01/14 18:31:36 layer Exp $

(in-package :user)

(defvar *trek-data* nil)
(defvar *raw-data* nil)

(defun make-keyword (x)
  (read-from-string
   (string-downcase
    (format nil ":~A" x))))

(defun my-type-of (x)
  (typecase x
    (string 'string)
    (number 'number)
    (otherwise (type-of x))))

(defun get-trek (key &optional prop)
  (when prop (setf key (list key prop)))
  (gethash key *trek-data*))

(defun collect-data ()
  (let ((i 0) prop props val)
    (setf *trek-data* (make-hash-table :test #'equal))
    (dolist (episode *raw-data*)
      (dolist (item episode)
	(pushnew (setf prop (make-keyword (first item))) props)
	(setf (gethash (list i prop) *trek-data*)
	      (if (null (cddr item))
		  (second item)
		(cdr item))))
      (incf i))
    (setf (gethash :props *trek-data*) props)
    (setf (gethash :items *trek-data*) i)
    (dolist (prop props)
      (let (types)
	(dotimes (item i)
	  (pushnew (my-type-of (gethash (list item prop) *trek-data*)) types))
	(setf (gethash prop *trek-data*) types)
	(push (cons prop types) val)
	))
    (setf (gethash :types *trek-data*) val)

    val))


(setf *raw-data* 
      (quote

       ( ( (TITLE "Where No Man Has Gone Before") (AUTHOR "Peeples" "Sam")
	   (DIRECTOR "James Goldstone") (SEASON 1) (POSITION-IN-SEASON 1)
	   (ABSOLUTE-NUMBER 1)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "Scott"
	    "Sulu"
	    "Lieutentant Commander Gary Mitchell"
	    "Dr. Elizabeth Dehner"
	    "Lieutenant Lee Kelso"
	    "Dr. Piper"
	    "Yeoman Smith"
	    "Lieutenant Alden")
	   (CAST ("Gary" "Gary Lockwood") ("Dehner" "Sally Kellerman"))
	   (LOCATIONS "Delta Vega") (STAR-DATES 1312.4)
	   (SHIPS "S.S. Valiant")
	   (ABSTRACT
	    "Second pilot.  In passing the energy barrier at the edge of the"
	    "galaxy, Gary Mitchell acquires powers which threaten the ship."
	    "Kirk is forced to kill his first officer and best friend.") )
	 ( (TITLE "Corbomite Maneuver") (AUTHOR "Sohl" "Jerry") (SEASON 1)
	   (POSITION-IN-SEASON 2) (ABSOLUTE-NUMBER 2)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Sulu"
	    "Scott"
	    "Uhura"
	    "Yeoman Janice Rand"
	    "Lieutenant Dave Bailey"
	    "Balok") (LOCATIONS) (STAR-DATES 1512.2) (SHIPS)
	   (ABSTRACT
	    "The Enterprise destroys a warning buoy and is threatened by an alien"
	    "vessel.  Kirk bluffs Balok by threatening to destroy both ships with"
	    "a Corbomite weapon.  The child-like alien manipulating the puppet,"
	    "Balok, was testing the peacefulness of the Federation.") )
	 ( (TITLE "Mudd's Women") (AUTHOR "Kandel" "Stephen") (SEASON 1)
	   (POSITION-IN-SEASON 3) (ABSOLUTE-NUMBER 3)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Sulu"
	    "Uhura"
	    "Harry Mudd"
	    "Harry Fenton Mudd"
	    "Eve McHuron"
	    "Magda Kovas"
	    "Ruth Bonaventure"
	    "Ben Childress"
	    "Lieutenant John Farrell"
	    "Gossett"
	    "Benton") (CAST ("Mudd" "Roger C. Carmel")) (LOCATIONS)
	   (STAR-DATES 1329.8) (SHIPS)
	   (ABSTRACT
	    "When Harry Mudd's ship is destroyed, he and three women are beamed"
	    "aboard.  Harry tries to trade the women, who have been treated with"
	    "the Venus drug, to miners in exchange for their help in freeing him.") )
	 ( (TITLE "Enemy Within") (AUTHOR "Matheson" "Richard") (SEASON 1)
	   (POSITION-IN-SEASON 4) (ABSOLUTE-NUMBER 4)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "Scott"
	    "McCoy"
	    "Sulu"
	    "Yeoman Janice Rand"
	    "Lieutenant John Farrell"
	    "Technician Fisher"
	    "Technician Wilson") (LOCATIONS) (STAR-DATES 1672.1) (SHIPS)
	   (ABSTRACT
	    "A transporter malfunction divides Kirk into two people, a lamb"
	    "and a wolf.  Each is a physically complete individual but both are"
	    "lacking in mental and emotional factors.  Both halves must be"
	    "reunited in time to save a stranded landing party led by Sulu.") )
	 ( (TITLE "Man Trap") (AUTHOR "Johnson" "George Clayton") (SEASON 1)
	   (POSITION-IN-SEASON 5) (ABSOLUTE-NUMBER 5)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Sulu"
	    "Uhura"
	    "Yeoman Janice Rand"
	    "Nancy Crater"
	    "Pyne"
	    "M113 Monster"
	    "Salt Vampire"
	    "Professor Robert Crater"
	    "Darnell"
	    "Green") (CAST ("Crater" "Alfred Ryder")) (LOCATIONS M-113)
	   (STAR-DATES 1513.1) (SHIPS)
	   (ABSTRACT
	    "A shape-changing Salt Vampire kills crew members for their salt.  One"
	    "of the shapes it takes is an old love of McCoy's.") )
	 ( (TITLE "Naked Time") (AUTHOR "Black" "John D.F.") (SEASON 1)
	   (POSITION-IN-SEASON 6) (ABSOLUTE-NUMBER 6)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Sulu"
	    "Uhura"
	    "Chapel"
	    "Lieutenant Kevin Riley"
	    "Joe Tormolen"
	    "Dr. Harrison"
	    "Lieutenant Brent") (LOCATIONS "Psi 2000") (STAR-DATES 1704.2) (SHIPS)
	   (ABSTRACT
	    "The Psi 2000 virus comes aboard from a ship where everyone went"
	    "beserk and died.  It makes the Enterprise crew express emotions."
	    "Sulu fences.  Chapel loves Spock.  Spock cries.  Kirk loves his ship."
	    "A cure is found.  The ship time-travels back several days.") )
	 ( (TITLE "Charlie X") (AUTHOR "Fontana" "Dorothy") (SEASON 1)
	   (POSITION-IN-SEASON 7) (ABSOLUTE-NUMBER 7)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Uhura"
	    "Yeoman Janice Rand"
	    "Charlie Evans"
	    "Thasian"
	    "Yeoman 3/C Tina Lawton"
	    "Captain Ramart"
	    "Tom Nellis") (CAST ("Charlie X" "Robert Walker, Jr."))
	   (LOCATIONS "Colony Alpha Five" "Thasus") (STAR-DATES 1533.6)
	   (SHIPS "Antares")
	   (ABSTRACT
	    "Charlie X is rescued as a sole survivor.  They discover him"
	    "to have superpowers given him by the aliens in order to keep"
	    "him alive.  As an adolescent, his powers disturb his emotional"
	    "growth and the aliens return to take him back to Thasus.") )
	 ( (TITLE "Balance of Terror") (AUTHOR "Schneider" "Paul") (SEASON 1)
	   (POSITION-IN-SEASON 8) (ABSOLUTE-NUMBER 8)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Specialist 2/C Angela Martine"
	    "Specialist Robert Tomlinson"
	    "Uhura"
	    "Yeoman Janice Rand"
	    "Scott"
	    "Sulu"
	    "Romulan Commander"
	    "Lieutenant Andrew Stiles"
	    "Decius"
	    "Centurion"
	    "Commander Hanson"
	    "Crewman Fields"
	    "Crewman Brenner") (LOCATIONS "Earth Outposts" "Romulan Neutral Zone")
	   (STAR-DATES 1709.2) (SHIPS "Romulan Vessel")
	   (ABSTRACT
	    "During a crew wedding, the Enterprise comes across destroyed"
	    "outposts.  They chase a Romulan vessel into the neutral zone and"
	    "finally destroy it.") )
	 ( (TITLE "What Are Little Girls Made Of") (AUTHOR "Bloch" "Robert") (SEASON 1)
	   (POSITION-IN-SEASON 9) (ABSOLUTE-NUMBER 9)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "Uhura"
	    "Chapel"
	    "Dr. Roger Korby"
	    "Andrea"
	    "Ruk"
	    "Dr. Brown"
	    "Matthews"
	    "Rayburn") (CAST ("Ruk" "Lurch from the Addams Family"))
	   (LOCATIONS "Exo III") (STAR-DATES 2712.4) (SHIPS)
	   (ABSTRACT
	    "In trying to rescue Dr. Roger Korby, Christine Chapel's fiancee,"
	    "they find he has become an android in order to survive.  He attempts"
	    "to convert Kirk also but Kirk turns the android Andrea against him.") )
	 ( (TITLE "Dagger of the Mind") (AUTHOR "Wincelberg" "Shimon (S. Bar David)")
	   (SEASON 1) (POSITION-IN-SEASON 10) (ABSOLUTE-NUMBER 10)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Uhura"
	    "Dr. Tristan Adams"
	    "Dr. Simon van Gelder"
	    "Woodward"
	    "Dr. Helen Noel"
	    "Lethe"
	    "Ensign Berkeley") (CAST ("Dr. Adams" "James Gregory"))
	   (LOCATIONS "Tantalus Five Penal Colony") (STAR-DATES 2715.1) (SHIPS)
	   (ABSTRACT
	    "Kirk investigates the charges of a former officer of an insane"
	    "asylum that the head, Dr. Adams, is torturing people.  Adams puts"
	    "Kirk onto the Tantalus machine but Kirk is able to escape.") )
	 ( (TITLE "Miri") (AUTHOR "Spies" "Adrian") (SEASON 1) (POSITION-IN-SEASON 11)
	   (ABSOLUTE-NUMBER 11)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Yeoman Janice Rand"
	    "Miri"
	    "Jahn"
	    "Lieutenant John Farrell")
	   (CAST ("Miri" "Kim Darby") ("Jahn" "Michael J. Pollard")) (LOCATIONS)
	   (STAR-DATES 2713.5) (SHIPS)
	   (ABSTRACT
	    "A planet of children carry a disease which kills them at adolescence."
	    "The disease spreads to the landing party and McCoy finds the cure.") )
	 ( (TITLE "Conscience of the King") (AUTHOR "Trivers" "Barry") (SEASON 1)
	   (POSITION-IN-SEASON 12) (ABSOLUTE-NUMBER 12)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Dr. Thomas Leighton"
	    "Anton Karidian"
	    "Uhura"
	    "Yeoman Janice Rand"
	    "King Duncan"
	    "Hamlet"
	    "Lenore Karidian"
	    "Lieutenant Kevin Riley"
	    "Lieutenant Leslie"
	    "Lieutenant Matson"
	    "Martha Leighton") (LOCATIONS "Tarsus IV") (STAR-DATES 2817.6) (SHIPS)
	   (ABSTRACT
	    "Kirk is obsessed to find out if the actor Karidian is also Kodos,"
	    "the ex-governor of Tarsus IV, who massacred part of the population"
	    "so that the rest could survive on supplies until rescue ships came.") )
	 ( (TITLE "Galileo Seven")
	   (AUTHOR "Wincelberg" "Shimon (S. Bar-David)" "Crawford" "Oliver") (SEASON 1)
	   (POSITION-IN-SEASON 13) (ABSOLUTE-NUMBER 13)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Sulu"
	    "Uhura"
	    "Lieutenant Boma"
	    "Gaetano"
	    "Latimer"
	    "Lieutenant Commander Kelowitz"
	    "Yeoman Mears"
	    "High Commissioner Ferris")
	   (LOCATIONS "Makus III" "Murasaki 312" "Taurus II") (STAR-DATES 2821.5)
	   (SHIPS)
	   (ABSTRACT
	    "Spock has his first command, the Galileo shuttle, to investigate a"
	    "quasarlike formation.  The Galileo crashlands and Spock's Vulcan"
	    "behavior emotionally isolates him from the rest of the survivors.") )
	 ( (TITLE "Court-Martial")
	   (AUTHOR "Mankiewicz" "Don M." "Carabatsos" "Stephen W.") (SEASON 1)
	   (POSITION-IN-SEASON 14) (ABSOLUTE-NUMBER 14)
	   (CHARACTERS
	    "Kirk"
	    "McCoy"
	    "Spock"
	    "Sulu"
	    "Uhura"
	    "Captain Chandra"
	    "Commodore Stone"
	    "Samuel T. Cogley"
	    "Lieutenant Areel Shaw"
	    "Lieutenant Commander Benjamin Finney"
	    "Jamie Finney"
	    "Lieutenant Hansen"
	    "Timothy"
	    "Captain Krasnowsky")
	   (CAST ("Stone" "Percy Rodriguez") ("Cogley" "Elisha Cook, Jr."))
	   (LOCATIONS "Starbase 12") (STAR-DATES 2950.1) (SHIPS)
	   (ABSTRACT
	    "Kirk is court-martialed for making a faulty command decision in"
	    "jettisoning a pod containing a crew member too soon.  Spock refuses"
	    "to believe the evidence and discovers Finney to have faked his own"
	    "death to discredit Kirk.") )
	 ( (TITLE "Menagerie") (AUTHOR "Roddenberry" "Gene") (SEASON 1)
	   (POSITION-IN-SEASON 15) (ABSOLUTE-NUMBER 15)
	   (CHARACTERS
	    "Capt. Christopher Pike"
	    "Number One (1)"
	    "Spock"
	    "Vina"
	    "Green Orion Girl"
	    "Uhura"
	    "Scotty"
	    "Kirk"
	    "McCoy"
	    "Dr. Phil"
	    "Commodore Mendez"
	    "Keeper"
	    "Chief Humbolt")
	   (CAST
	    ("Pike" "Jeffrey Hunter")
	    ("Number One" "Majel Barret")
	    ("Mendez" "Malachi Throne"))
	   (LOCATIONS "Rigel VII" "Earth" "Orion" "Talos IV" "Starbase 11")
	   (STAR-DATES 3012.4) (SHIPS)
	   (ABSTRACT
	    "Contains first pilot.  Spock steals the Enterprise to return"
	    "Captain Pike to a quarantined planet, Talos IV, where he can spend"
	    "the rest of his cripplied life maintained in illusions.") )
	 ( (TITLE "Shore Leave") (AUTHOR "Sturgeon" "Theodore") (SEASON 1)
	   (POSITION-IN-SEASON 16) (ABSOLUTE-NUMBER 16)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Sulu"
	    "Uhura"
	    "Ruth"
	    "Caretaker"
	    "Finnegan"
	    "Alice in Wonderland"
	    "Specialist 2/C Angela Martine"
	    "Teller"
	    "Yeoman Tonia Barrows"
	    "Lieutenant Esteban Rodriguez"
	    "Don Juan"
	    "Samurai"
	    "White Rabbit"
	    "Black Knight") (LOCATIONS) (STAR-DATES 3025.3) (SHIPS)
	   (ABSTRACT
	    "A Shore Leave planet turns out to be one where fantasies come true."
	    "Some fantasies, however, turn out to be dangerous.  Kirk comes upon"
	    "Ruth and his old Academy nemesis, Finnegan.  McCoy finds a white"
	    "rabbit.") )
	 ( (TITLE "Squire of Gothos") (AUTHOR "Schneider" "Paul") (SEASON 1)
	   (POSITION-IN-SEASON 17) (ABSOLUTE-NUMBER 17)
	   (CHARACTERS
	    "Trelane"
	    "Kirk"
	    "Spock"
	    "Scott"
	    "McCoy"
	    "Sulu"
	    "Uhura"
	    "Lieutenant Karl Jaeger"
	    "Lieutenant DeSalle"
	    "Yeoman Teresa Ross") (CAST ("Trelane" "William Campbell"))
	   (LOCATIONS "Gothos" "Colony Beta Six") (STAR-DATES 2124.5) (SHIPS)
	   (ABSTRACT
	    "Some of the crew are captured by a powerful alien, Trelane, who"
	    "creates fantasy illusions.  When Kirk escapes, Trelane recaptures"
	    "him and hunts him down for punishment.  Trelane turns out to be"
	    "a child whose parents come for him.") )
	 ( (TITLE "Arena") (AUTHOR "Coon" "Gene L.") (SEASON 1) (POSITION-IN-SEASON 18)
	   (ABSOLUTE-NUMBER 18)
	   (CHARACTERS
	    "Kirk"
	    "Gorn"
	    "Metron"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Sulu"
	    "Uhura"
	    "Lieutenant O'Herilihy"
	    "Lieutenant Commander Kelowitz"
	    "Lieutenant Harold"
	    "Lieutenant Lang"
	    "Lieutenant DePaul") (LOCATIONS "Cestus Three") (STAR-DATES 3046.2)
	   (SHIPS "Gorn vessel")
	   (ABSTRACT
	    "The Enterprise chases a ship which has destroyed a base into alien"
	    "territory.  The captains of both ships are taken by the Metrons and"
	    "forced to fight.  Kirk conquers but does not kill the Gorn.") )
	 ( (TITLE "Alternative Factor") (AUTHOR "Ingalls" "Don") (SEASON 1)
	   (POSITION-IN-SEASON 19) (ABSOLUTE-NUMBER 19)
	   (CHARACTERS
	    "Lazarus"
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Commodore Barstow"
	    "Uhura"
	    "Lieutenant Charlene Masters"
	    "Lieutenant Leslie") (LOCATIONS) (STAR-DATES 3087.6) (SHIPS)
	   (ABSTRACT
	    "In investigating a planet thought to be the center of a moment of"
	    "non-existence for the universe, Kirk finds an alien, Lazarus, who is"
	    "chasing his insane alternate between universes.  The universe is"
	    "saved by the sacrifice of the sane Lazarus in trapping himself in"
	    "a tunnel between universes for eternity.") )
	 ( (TITLE "Tomorrow is Yesterday") (AUTHOR "Fontana" "Dorothy") (SEASON 1)
	   (POSITION-IN-SEASON 20) (ABSOLUTE-NUMBER 20)
	   (CHARACTERS
	    "Kirk"
	    "Sulu"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Captain John Christopher"
	    "Uhura"
	    "Air Police Sergeant"
	    "Colonel Fellini"
	    "Technician Webb"
	    "Transporter Chief Kyle") (LOCATIONS) (STAR-DATES 3113.2) (SHIPS)
	   (ABSTRACT
	    "The Enterprise accidentally goes back through time to the 1960's"
	    "where they are identified as a UFO.  They capture a chase pilot"
	    "and consider keeping him but find he will father an important"
	    "astronaut and return him to Earth before the sighting.") )
	 ( (TITLE "Return of the Archons") (AUTHOR "Sobelman" "Boris") (SEASON 1)
	   (POSITION-IN-SEASON 21) (ABSOLUTE-NUMBER 21)
	   (CHARACTERS
	    "Landru"
	    "Kirk"
	    "Spock"
	    "Reger"
	    "Marplon"
	    "Tula"
	    "McCoy"
	    "Sulu"
	    "Lindstrom"
	    "Scott"
	    "Uhura"
	    "Tamar"
	    "Hacom"
	    "Bilar"
	    "Lieutenant Leslie"
	    "Lieutenant O'Neil") (LOCATIONS "Beta III") (STAR-DATES 3156.2)
	   (SHIPS "Archon")
	   (ABSTRACT
	    "A paradise exists controlled by a computer Landru.  It provides for"
	    "release of agressions through organized riots.  McCoy is absorbed"
	    "into Landru until Kirk can destroy it.") )
	 ( (TITLE "Taste of Armageddon") (AUTHOR "Coon" "Gene L." "Hammer" "Robert")
	   (SEASON 1) (POSITION-IN-SEASON 22) (ABSOLUTE-NUMBER 22)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Uhura"
	    "Ambassador Robert Fox"
	    "Anan 7"
	    "Sar 6"
	    "Mea 3"
	    "Yeoman Tamura"
	    "Lieutenant Galloway"
	    "Lieutenant DePaul")
	   (CAST ("Anan 7" "David Opotashu") ("Mea" "Grace on Hill Street Blues"))
	   (LOCATIONS "Eminiar VII" "Vendikar") (STAR-DATES 3192.1) (SHIPS)
	   (ABSTRACT
	    "Ambassador Fox is trying to open diplomatic relations with Eminiar"
	    "Seven but finds he has been identified as casualities of a war"
	    "waged with computers.  Kirk refuses to have his ship destroyed on"
	    "computer control and destroys the computers so that war will be real.") )
	 ( (TITLE "Space Seed") (AUTHOR "Coon" "Gene L." "Wilbur" "Carey") (SEASON 1)
	   (POSITION-IN-SEASON 23) (ABSOLUTE-NUMBER 23)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Khan Noonian Singh"
	    "Uhura"
	    "Scott"
	    "Lieutenant Marla McGivers"
	    "Lieutenant Spinelli"
	    "Joaquin") (CAST ("Khan" "Ricardo Montalban")) (LOCATIONS CETI ALPHA V)
	   (STAR-DATES 3141.9) (SHIPS "Botany Bay")
	   (ABSTRACT
	    "The Enterprise encounters a sleeper ship with many sleepers still"
	    "alive.  The awakened sleepers try to capture the Enterprise but are"
	    "defeated.  Khan, a product of twentieth century genetic engineering,"
	    "and his people are left on Ceta Alpha V.") )
	 ( (TITLE "This Side of Paradise") (AUTHOR "Fontana" "Dorothy") (SEASON 1)
	   (POSITION-IN-SEASON 24) (ABSOLUTE-NUMBER 24)
	   (CHARACTERS
	    "Spock"
	    "Kirk"
	    "Leila Kalomi"
	    "McCoy"
	    "Sulu"
	    "Elias Sandoval"
	    "Uhura"
	    "Leslie"
	    "Lieutenant Commander Kelowitz"
	    "Painter"
	    "Lieutenant DeSalle") (CAST ("Leila" "Jill St. John"))
	   (LOCATIONS "Omicron Ceti III") (STAR-DATES 3417.3) (SHIPS)
	   (ABSTRACT
	    "While trying to evacuate settlers from a colony endangered by"
	    "Berthold rays, the crew is infected by the spores which have pro-"
	    "tected the settlers.  The spores cause Spock to express his love for"
	    "Leila, an Academy botonist.  Kirk uses strong emotions to kill the"
	    "spores.") )
	 ( (TITLE "Devil in the Dark") (AUTHOR "Coon" "Gene L.") (SEASON 1)
	   (POSITION-IN-SEASON 25) (ABSOLUTE-NUMBER 25)
	   (CHARACTERS
	    "Horta"
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Chief Engineer Vanderberg"
	    "Lieutenant Commander Giotto"
	    "Ed Appel"
	    "Schmitter"
	    "Sam") (LOCATIONS "Janus VI") (STAR-DATES 3196.1) (SHIPS)
	   (ABSTRACT
	    "The crew hunts a creature which has been killing pergium miners."
	    "Spock discovers it is a mother Horta which has been taking revenge"
	    "for the eggs killed by the miners.") )
	 ( (TITLE "Errand of Mercy") (AUTHOR "Coon" "Gene L.") (SEASON 1)
	   (POSITION-IN-SEASON 26) (ABSOLUTE-NUMBER 26)
	   (CHARACTERS
	    "Organians"
	    "Kirk"
	    "Spock"
	    "Klingons"
	    "Commander Kor"
	    "Sulu"
	    "Uhura"
	    "Trefayne"
	    "Ayelborne"
	    "Claymare") (LOCATIONS "Organia") (STAR-DATES 3198.4) (SHIPS)
	   (ABSTRACT
	    "Kirk and Spock try to protect the Organians from the Klingons.  The"
	    "Organians seem undisturbed at the idea of war.  Klingon Commander"
	    "Kor invades and captures Kirk and Spock.  The war is suddenly averted"
	    "by the Organians who are powerful energy creatures who ban war.") )
	 ( (TITLE "City on the Edge of Forever") (AUTHOR "Elison" "Harlan") (SEASON 1)
	   (POSITION-IN-SEASON 27) (ABSOLUTE-NUMBER 27)
	   (CHARACTERS
	    "Sulu"
	    "Uhura"
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Edith Keeler"
	    "Lieutenant Galloway"
	    "Rodent"
	    "Chapel"
	    "Guardian of Forever") (CAST ("Edith Keeler" "Joan Collins"))
	   (LOCATIONS "Earth") (STAR-DATES 3134.) (SHIPS)
	   (ABSTRACT
	    "McCoy goes through the Guardian of Forever and changes the future."
	    "Kirk and Spock return to Earth, 1930's, to stop him.  Kirk falls"
	    "in love with Edith Keeler who must die for the future to resume.") )
	 ( (TITLE "Operation -- Annihilate") (AUTHOR "Carabatsos" "Stephen W.")
	   (SEASON 1) (POSITION-IN-SEASON 28) (ABSOLUTE-NUMBER 28)
	   (CHARACTERS
	    "Sam Kirk"
	    "Aurelan Kirk"
	    "Peter Kirk"
	    "Kirk"
	    "Scott"
	    "Sulu"
	    "Uhura"
	    "Chapel"
	    "Kartan"
	    "Yeoman Zahra Jamal"
	    "Spock"
	    "McCoy") (LOCATIONS "Deneva") (STAR-DATES 3287.2) (SHIPS)
	   (ABSTRACT
	    "Deneva has been invaded by creatures which cause mass insanity."
	    "Kirk's brother and sister-in-law are among those killed by the"
	    "creatures.  Spock is affected and McCoy finds a cure which"
	    "results in Spock's temporary blindness.") )
	 ( (TITLE "Catspaw") (AUTHOR "Bloch" "Robert") (SEASON 2)
	   (POSITION-IN-SEASON 1) (ABSOLUTE-NUMBER 29)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Sulu"
	    "Sylvia"
	    "Korob"
	    "Scott"
	    "Uhura"
	    "Chekov"
	    "Lieutenant DeSalle"
	    "Jackson"
	    "Lieutenant Kyle"
	    "Witch") (LOCATIONS "Pyris VII") (STAR-DATES 3018.2) (SHIPS)
	   (ABSTRACT
	    "The landing party encounters witches who transform Sulu and Scotty"
	    "into zombies.  Sylvia tries to entice Kirk but he is able to destroy"
	    "her magic wand transmuter and the witches are reduced to their"
	    "original forms which are too fragile to survive.") )
	 ( (TITLE "Metamorphosis") (AUTHOR "Coon" "Gene L.") (SEASON 2)
	   (POSITION-IN-SEASON 2) (ABSOLUTE-NUMBER 30)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Zefrem Cochrane"
	    "Companion"
	    "Nancy Hedford"
	    "Sulu"
	    "Uhura"
	    "Scott") (CAST ("Nancy" "Eleanor Donoghue")) (LOCATIONS "Gamma Canaris N")
	   (STAR-DATES 3219.4) (SHIPS "Shuttlecraft Galileo")
	   (ABSTRACT
	    "Zefrem Cochrane, discoverer of the warp drive, is discovered"
	    "on Gamma Canaris N.  He stays when the Companion merges with"
	    "Nancy Heaford.") )
	 ( (TITLE "Friday's Child") (AUTHOR "Fontana" "Dorothy") (SEASON 2)
	   (POSITION-IN-SEASON 3) (ABSOLUTE-NUMBER 31)
	   (CHARACTERS
	    "Klingon"
	    "Eleen"
	    "High Teer Akaar"
	    "Kras"
	    "McCoy"
	    "Kirk"
	    "Spock"
	    "Leonard James Akaar") (CAST ("Eleen" "Julie Newmar"))
	   (LOCATIONS "Capella IV") (STAR-DATES 3497.2) (SHIPS)
	   (ABSTRACT
	    "Kirk interferes with Capellan tradition by saving Eleen, the"
	    "wife of the deposed High Teer Akaar.  The Klingon Kras"
	    "uses this situation to make them into hunted criminals.  McCoy"
	    "delivers Eleen's baby.  The Klingons kill the current Teer and"
	    "Eleen's son, Leonard James Akaar, is named ruler.") )
	 ( (TITLE "Who Mourns for Adonais")
	   (AUTHOR "Coon" "Gene L." "Ralston" "Gilbert A.") (SEASON 2)
	   (POSITION-IN-SEASON 4) (ABSOLUTE-NUMBER 32)
	   (CHARACTERS
	    "Apollo"
	    "Scott"
	    "Lieutenant Carolyn Palamas"
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Sulu"
	    "Uhura"
	    "Chekov"
	    "Lieutenant Kyle") (LOCATIONS "Pollux IV") (STAR-DATES 3468.1) (SHIPS)
	   (ABSTRACT
	    "The Enterprise is caught by a large green hand belonging to Apollo."
	    "He wants the crew to stay and worship him.  Lt. Palamas falls in"
	    "love with Apollo but helps Kirk destroy his temple.") )
	 ( (TITLE "Amok Time") (AUTHOR "Sturgeon" "Theodore") (SEASON 2)
	   (POSITION-IN-SEASON 5) (ABSOLUTE-NUMBER 33)
	   (CHARACTERS
	    "Spock"
	    "Kirk"
	    "McCoy"
	    "T'Pring"
	    "T'Pau"
	    "Chapel"
	    "Sulu"
	    "Uhura"
	    "Chekov"
	    "Stonn") (LOCATIONS "Vulcan") (STAR-DATES 3372.7) (SHIPS)
	   (ABSTRACT
	    "Spock suffers from pon farr and is taken by Kirk to Vulcan to"
	    "marry.  His bondmate issues a challenge and chooses Kirk as her"
	    "champion.  Spock thinks he has killed Kirk in combat but McCoy"
	    "has saved Kirk with a death-simulating injection.") )
	 ( (TITLE "Doomsday Machine") (AUTHOR "Spinrad" "Norman") (SEASON 2)
	   (POSITION-IN-SEASON 6) (ABSOLUTE-NUMBER 34)
	   (CHARACTERS
	    "Commodore Matthew Decker"
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scott"
	    "Sulu"
	    "Lieutenant Palmer"
	    "Elliot"
	    "Washburn"
	    "Lieutenant Kyle"
	    "Russ"
	    "Montgomery") (CAST ("Decker" "William Windom")) (LOCATIONS)
	   (STAR-DATES 4202.9) (SHIPS "Constellation")
	   (ABSTRACT
	    "The Enterprise finds a crippled starship containing only Commodore"
	    "Decker.  His crew was killed by a planet-destroyer.  He endan-"
	    "gers the Enterprise but is killed in a suicide mission.  Kirk"
	    "uses the damaged starship to kill the Doomsday Machine.") )
	 ( (TITLE "Wolf in the Fold") (AUTHOR "Bloch" "Robert") (SEASON 2)
	   (POSITION-IN-SEASON 7) (ABSOLUTE-NUMBER 35)
	   (CHARACTERS "Scotty" "Hengist" "Kirk" "Spock")
	   (CAST ("Hengist" "John Fiedler")) (LOCATIONS "Argelius Two")
	   (STAR-DATES 3614.9) (SHIPS)
	   (ABSTRACT
	    "Scotty is accused of murdering a woman on Argelius Two.  He is tried"
	    "by the planet authorities but another woman dies.  Spock shows that"
	    "the murderer is an entity which has murdered through space and time.") )
	 ( (TITLE "Changeling") (AUTHOR "Lucas" "John Meredyth") (SEASON 2)
	   (POSITION-IN-SEASON 8) (ABSOLUTE-NUMBER 36)
	   (CHARACTERS
	    "Scott"
	    "Nomad"
	    "Kirk"
	    "Spock"
	    "Tan-Ru"
	    "Jackson Roykirk"
	    "McCoy"
	    "Sulu"
	    "Uhura"
	    "Chapel"
	    "Mr. Singh"
	    "Lieutenant Carlisle") (LOCATIONS "Malurian System") (STAR-DATES 3541.9)
	   (SHIPS)
	   (ABSTRACT
	    "An entire planetary system is destroyed by Nomad.  The Enterprise"
	    "takes Nomad on board and finds it is a space probe which has merged"
	    "with a more advanced probe and had its programming changed to cause"
	    "it to sterilize lifeforms.  Kirk uses logic to cause it to self-"
	    "destruct.") )
	 ( (TITLE "Apple") (AUTHOR "Coon" "Gene L." "Ehrlich" "Max") (SEASON 2)
	   (POSITION-IN-SEASON 9) (ABSOLUTE-NUMBER 37)
	   (CHARACTERS
	    "Vaal"
	    "Akuta"
	    "Makora"
	    "Spock"
	    "Kirk"
	    "McCoy"
	    "Scott"
	    "Chekov"
	    "Lieutenant Kyle"
	    "Yeoman Martha Landon"
	    "Sayana"
	    "Hendorff"
	    "Ensign Mallory"
	    "Kaplan") (LOCATIONS "Gamma Trianguli VI") (STAR-DATES 3715.3) (SHIPS)
	   (ABSTRACT
	    "A paradise on a dangerous planet where rocks explode and flowers"
	    "kill is run by a computer called Vaal.  Kirk destroys the computer"
	    "and makes the inhabitants run their own lives.") )
	 ( (TITLE "Mirror, Mirror") (AUTHOR "Bixby" "Jerome") (SEASON 2)
	   (POSITION-IN-SEASON 10) (ABSOLUTE-NUMBER 38)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Uhura"
	    "Sulu"
	    "Chekov"
	    "Lieutenant Marlena Moreau"
	    "Scott"
	    "Tharn"
	    "Farrell"
	    "Wilson"
	    "Lieutenant Kyle") (LOCATIONS "Halkan" "Alternate Universe")
	   (STAR-DATES 0) (SHIPS)
	   (ABSTRACT
	    "An ion storm causes the landing party to be beamed to a violent"
	    "alternate universe.  The alternate Spock helps them to return and"
	    "Kirk tries to make him take action to overthrow the Empire.") )
	 ( (TITLE "Deadly Years") (AUTHOR "Harmon" "David P.") (SEASON 2)
	   (POSITION-IN-SEASON 11) (ABSOLUTE-NUMBER 39)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Dr. Janet Wallace"
	    "Chekov"
	    "Commodore George Stocker"
	    "Romulans")
	   (LOCATIONS "Gamma Hydra IV" "Starbase Ten" "Romulan Neutral Zone")
	   (STAR-DATES 3478.2) (SHIPS "Romulan Vessels")
	   (ABSTRACT
	    "A landing party brings back an aging disease.  Commodore Stocker"
	    "takes command by convening a competency hearing for Kirk.  He"
	    "endangers the ship but McCoy finds the antidote in time for Kirk"
	    "to use his bluffing technique against the Romulans.") )
	 ( (TITLE "I, Mudd") (AUTHOR "Gerrold" "David" "Kandel" "Stephen") (SEASON 2)
	   (POSITION-IN-SEASON 12) (ABSOLUTE-NUMBER 40)
	   (CHARACTERS
	    "Harry Mudd"
	    "Stella Mudd"
	    "Norman"
	    "Scott"
	    "McCoy"
	    "Kirk"
	    "Spock"
	    "Sulu"
	    "Uhura"
	    "Chekov"
	    "Herman Series"
	    "Barbara Series"
	    "Maisie Series"
	    "Lieutenant Rowe"
	    "Ensign Jordan") (CAST ("Mudd" "Roger C. Carmel")) (LOCATIONS)
	   (STAR-DATES 4513.3) (SHIPS)
	   (ABSTRACT
	    "Harry Mudd has proclaimed himself emperor of a planet of androids"
	    "which is intending to rule the galaxy.  The androids intend to strand"
	    "Mudd as well as the Enterprise crew so he helps defeat the androids"
	    "by using illogical behavior.") )
	 ( (TITLE "Trouble with Tribbles") (AUTHOR "Gerrold" "David") (SEASON 2)
	   (POSITION-IN-SEASON 13) (ABSOLUTE-NUMBER 41)
	   (CHARACTERS
	    "Klingon"
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scotty"
	    "Uhura"
	    "Chekov"
	    "Mr. Lurie"
	    "Capt. Koloth"
	    "Neils Barras"
	    "Arn Darvon"
	    "Cyrano Jones")
	   (CAST ("Barras" "Patty Dukes TV father") ("Koloth" "William Campbell"))
	   (LOCATIONS "Space Station K-7") (STAR-DATES 4523.3) (SHIPS "Klingon Ship")
	   (ABSTRACT
	    "The Enterprise is called to Space Station K-7 to protect"
	    "quadrotricale from the Klingons.  Cyrano Jones introduces tribbles"
	    "to the crew.  The tribbles multiply over the ship and K-7,"
	    "poisoning themselves in the process and exposing a spy.") )
	 ( (TITLE "Bread and Circuses") (AUTHOR "Roddenberry" "Gene" "Coon" "Gene L.")
	   (SEASON 2) (POSITION-IN-SEASON 14) (ABSOLUTE-NUMBER 42)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "Chekov"
	    "Merrick"
	    "Uhura"
	    "Sulu"
	    "McCoy"
	    "Septimus"
	    "Flavius"
	    "Merikus"
	    "Claudius Marcus"
	    "Scotty"
	    "Drusilla") (LOCATIONS) (STAR-DATES 4040.7) (SHIPS "Beagle")
	   (ABSTRACT
	    "The Captain of the Beagle has been stranded on a Roman-like planet."
	    "His crew was killed in the circuses.  The landing party is captured"
	    "and sentenced to death.  Merrick suicides to save them.") )
	 ( (TITLE "Journey to Babel")
	   (AUTHOR "Fontana" "Dorothy")
	   (SEASON 2)
	   (POSITION-IN-SEASON 15)
	   (ABSOLUTE-NUMBER 43)
	   (CHARACTERS
	    "Spock"
	    "Kirk"
	    "Sarek"
	    "Amanda"
	    "McCoy"
	    "Uhura"
	    "Chekov"
	    "Chapel"
	    "Thelev"
	    "Shras"
	    "Gav"
	    "Lieutenant Josephs")
	   (CAST ("Sarek" "Mark Lenard") ("Amanda" "Jane Wyatt"))
	   (LOCATIONS "Babel")
	   (STAR-DATES 3842.3)
	   (SHIPS "Orion Vessel")
	   (ABSTRACT
	   "Spock's father, the Vulcan Ambassador, and mother join a group of"
	   "diplomats on their way to the Babel conference.  An Orion spy kills"
	   "the Tellerite ambassador to disrupt the conference.  Spock chooses"
	   "duty over donating blood to his father when Kirk is stabbed by the"
	   "spy.") )
	 ( (TITLE "Private Little War") (AUTHOR "Roddenberry" "Gene") (SEASON 2)
	   (POSITION-IN-SEASON 16) (ABSOLUTE-NUMBER 44)
	   (CHARACTERS
	    "Klingon"
	    "Nona"
	    "Tyree"
	    "McCoy"
	    "Kirk"
	    "Dr. M'Benga"
	    "Chapel"
	    "Spock") (LOCATIONS "Neural") (STAR-DATES 4211.4) (SHIPS)
	   (ABSTRACT
	    "Kirk and the Enterprise return to Neural after thirteen years to"
	    "find that the Klingons are arming part of the population to increase"
	    "unrest so that they can take over.  Kirk's friend's wife steals"
	    "his phaser but is killed by Klingon rebels.  Kirk's friend turns"
	    "to war.") )
	 ( (TITLE "Gamesters of Triskelion") (AUTHOR "Armen" "Margaret") (SEASON 2)
	   (POSITION-IN-SEASON 17) (ABSOLUTE-NUMBER 45)
	   (CHARACTERS
	    "Chekov"
	    "Kirk"
	    "Spock"
	    "Uhura"
	    "Galt"
	    "Thralls"
	    "Providers"
	    "Shahna"
	    "Lars"
	    "Tamoon") (LOCATIONS "Triskelion") (STAR-DATES 3211.7) (SHIPS)
	   (ABSTRACT
	    "Kirk, Uhura and Chekov are abducted in a transporter beam."
	    "They find themselves on M-24 Alpha's Triskelion.  They are to be"
	    "used as gladiators.  Spock finally finds where they have been taken"
	    "but the Enterprise is captured.  Kirk proposes a wager which he wins,"
	    "freeing not only the planet but the captive Thralls.") )
	 ( (TITLE "Obsession") (AUTHOR "Wallace" "Art") (SEASON 2)
	   (POSITION-IN-SEASON 18) (ABSOLUTE-NUMBER 46)
	   (CHARACTERS "Garrovick" "Kirk" "Spock") (LOCATIONS "Argus X" "Tycho IV")
	   (STAR-DATES 3619.2) (SHIPS "Farragut")
	   (ABSTRACT
	    "In his early career, half the crew of the Farragut was killed by a"
	    "blood-sucking cloud.  Kirk is obsessed with its capture.  It leaves"
	    "the Enterprise when it mistakenly tries to suck Spock's blood.  Kirk"
	    "follows it to Tycho IV and blows it up with an antimatter bomb.") )
	 ( (TITLE "Immunity Syndrome") (AUTHOR "Sabaroff" "Robert") (SEASON 2)
	   (POSITION-IN-SEASON 19) (ABSOLUTE-NUMBER 47)
	   (CHARACTERS "Kirk" "Spock" "McCoy") (LOCATIONS) (STAR-DATES 4308.8) (SHIPS)
	   (ABSTRACT
	    "The Enterprise encounters a large one-celled creature which is"
	    "destroying everything in its path.  The Enterprise acts as an"
	    "antibody and invades the creature.  Fatigue and depression affect"
	    "the crew.  Spock takes a shuttlecraft into the heart of the"
	    "creature and finds it is about to divide.  He kills it and returns"
	    "to the Enterprise as his life-support systems are about to go.") )
	 ( (TITLE "Piece of the Action") (AUTHOR "Coon" "Gene L." "Harmon" "David P.")
	   (SEASON 2) (POSITION-IN-SEASON 20) (ABSOLUTE-NUMBER 48)
	   (CHARACTERS "Kirk" "Spock" "McCoy" "Bela Oxmyx" "Jojo Krako")
	   (LOCATIONS "Iotia") (STAR-DATES 4598.) (SHIPS "Horizon")
	   (ABSTRACT
	    "When the Horizon visited the planet Iotia, someone left behind"
	    "a copy of Chicago Mobs of the Twenties.  The natives were captivated"
	    "and modeled their civilization after the book.  The Enterprise visits"
	    "one hundred years later.  Kirk starts fitting in and sets up a world"
	    "government behind one of the gangsters, leaving behind the impression"
	    "that the Federation will be expecting a piece of the action.") )
	 ( (TITLE "By Any Other Name") (AUTHOR "Fontana" "Dorothy" "Bixby" "Jerome")
	   (SEASON 2) (POSITION-IN-SEASON 21) (ABSOLUTE-NUMBER 49)
	   (CHARACTERS
	    "Kirk"
	    "Spock"
	    "McCoy"
	    "Scotty"
	    "Rojan"
	    "Kelinda"
	    "Hanar"
	    "Tomar"
	    "Kelvans") (LOCATIONS "Kelva") (STAR-DATES 4657.5) (SHIPS)
	   (ABSTRACT
	    "Our galaxy is being investigated for possible colonization by"
	    "the Kelvan Empire.  Rojan and his followers take over the"
	    "Enterprise, transforming the crew into tetrahedral blocks."
	    "The crew save the ship by taking advantage of the Kelvans unfam-"
	    "iliarity with human emotions and helps the Kelvans find a planet.") )
	 ( (TITLE "Return to Tomorrow") (AUTHOR "Roddenberry" "Gene") (SEASON 2)
	   (POSITION-IN-SEASON 22) (ABSOLUTE-NUMBER 50)
	   (CHARACTERS
	    "McCoy"
	    "Kirk"
	    "Spock"
	    "Dr. Anne Mulhall"
	    "Chapel"
	    "Sargon"
	    "Henoch"
	    "Thalassa") (CAST ("Anne" "Diana Muldaur")) (LOCATIONS)
	   (STAR-DATES 4767.3) (SHIPS)
	   (ABSTRACT
	    "Three advanced minds lure the ship to a dead planet and ask to use"
	    "three bodies to build androids for their minds.  The mind in Spock's"
	    "body decides to keep it and the others kill him, thinking they are"
	    "also killing Spock.  The other two decide that bodies are too"
	    "tempting.") )
	 ( (TITLE "Patterns of Force") (AUTHOR "Lucas" "John Meredyth") (SEASON 2)
	   (POSITION-IN-SEASON 23) (ABSOLUTE-NUMBER 51)
	   (CHARACTERS "Kirk" "Spock" "John Gill" "Melakon" "McCoy")
	   (LOCATIONS "Ekos" "Zeon") (STAR-DATES 2534.) (SHIPS)
	   (ABSTRACT
	    "John Gill has violated the Prime Directive by recreating Nazi"
	    "Germany to get an efficient and united government.  Gill is drugged"
	    "and becomes a figurehead.  Kirk intervenes and Gill is able to"
	    "denounce the government before he is killed.") )
	 ( (TITLE "Ultimate Computer") (AUTHOR "Fontana" "Dorothy") (SEASON 2)
	   (POSITION-IN-SEASON 24) (ABSOLUTE-NUMBER 52)
	   (CHARACTERS
	    "Kirk"
	    "M-5 Computer"
	    "Dr. Richard Daystrom"
	    "Commodore Robert Wesley") (LOCATIONS) (STAR-DATES 4729.4)
	   (SHIPS "Excalibur")
	   (ABSTRACT
	    "Daystrom invents a command computer based on his own mental"
	    "the past and convinces Starfleet to let him try out a new M-5 computer"
	    "patterns.  During a war game, M-5 takes over and destroys the other"
	    "ships.  Kirk uses logic to confuse the computer so that Scotty and"
	    "Spock can pull its plug.") )
	 ( (TITLE "Omega Glory") (AUTHOR "Roddenberry" "Gene") (SEASON 2)
	   (POSITION-IN-SEASON 25) (ABSOLUTE-NUMBER 53)
	   (CHARACTERS "Capt. Ronald Tracey" "Kohms" "Yangs" "Kirk" "Spock" "McCoy")
	   (LOCATIONS "Omega IV") (STAR-DATES 0) (SHIPS "Exeter")
	   (ABSTRACT
	    "The crew of the Exeter has been reduced to powder by a virus."
	    "The Exeter captain thought he had found immortality and violated the"
	    "Prime Directive by taking sides between the Yangs and the Coms."
	    "The groups turn out to be derived from Yankees and Communists.") )
	 ( (TITLE "Assignment Earth") (AUTHOR "Roddenberry" "Gene" "Wallace" "Art")
	   (SEASON 2) (POSITION-IN-SEASON 26) (ABSOLUTE-NUMBER 54)
	   (CHARACTERS "Kirk" "Spock" "Gary Seven" "Roberta Lincoln" "Isis")
	   (CAST ("Gary Seven" "Robert Lansing") ("Roberta Lincoln" "Terri Garr"))
	   (LOCATIONS "Earth") (STAR-DATES 1968) (SHIPS)
	   (ABSTRACT
	    "New series pilot.  While in the past, the Enterprise intercepts the"
	    "transporter beam of Gary Seven and his cat, Isis.  Gary Seven gets"
	    "them to help him stop the deployment of nuclear weapons by the US"
	    "in space.") )
	 ( (TITLE "Spectre of the Gun") (AUTHOR "Coon" "Gene L.") (SEASON 3)
	   (POSITION-IN-SEASON 1) (ABSOLUTE-NUMBER 55)
	   (CHARACTERS "Chekov" "Sylvia" "Melkotian" "Kirk" "Spock" "Scotty" "McCoy")
	   (LOCATIONS) (STAR-DATES 4385.3) (SHIPS)
	   (ABSTRACT
	    "Because they have ignored a warning buoy, some of the crew are"
	    "transported to a western town where they are to play the part of"
	    "the losing outlaws.  Spock mindmelds with them to protect them from"
	    "the illusion.  Because Kirk refuses to kill, the Melkokians let them"
	    "go.") )
	 ( (TITLE "Elaan of Troyius") (AUTHOR "Lucas" "John Meredyth") (SEASON 3)
	   (POSITION-IN-SEASON 2) (ABSOLUTE-NUMBER 56)
	   (CHARACTERS
	    "Kirk"
	    "Elaan"
	    "Petri"
	    "Kryton"
	    "Klingon"
	    "Troyian"
	    "Spock"
	    "McCoy"
	    "Uhura") (CAST ("Elaan" "France Nguyen"))
	   (LOCATIONS "Troyius" "Elas" "Tellun") (STAR-DATES 4372.5)
	   (SHIPS "Klingon vessel")
	   (ABSTRACT
	    "The Enterprise is transporting the bride of an arranged marriage."
	    "She cries and her tears have the power to make Kirk fall in love"
	    "with her.  His duty is stronger and he sends her to her marriage."
	    "Kirk uses her dilithium necklace to save the ship from Klingons.") )
	 ( (TITLE "Paradise Syndrome") (AUTHOR "Armen" "Margaret") (SEASON 3)
	   (POSITION-IN-SEASON 3) (ABSOLUTE-NUMBER 57)
	   (CHARACTERS "Kirk" "Kirok" "Spock" "McCoy" "Miramanee" "Goro" "Salish")
	   (LOCATIONS) (STAR-DATES 4842.6) (SHIPS)
	   (ABSTRACT
	    "Kirk loses his memory and is stranded on a planet which will be"
	    "destroyed by an asteroid.  Spock tries to destroy the asteroid but"
	    "returns in time to learn how to operate the asteroid-deflector."
	    "He is not in time to save Kirk's wife, Miramanee, from being killed.") )
	 ( (TITLE "Enterprise Incident") (AUTHOR "Fontana" "Dorothy") (SEASON 3)
	   (POSITION-IN-SEASON 4) (ABSOLUTE-NUMBER 58)
	   (CHARACTERS "Romulan" "Kirk" "Spock" "Romulan Commander") (LOCATIONS)
	   (STAR-DATES 5031.3) (SHIPS "Romanulan Vessel")
	   (ABSTRACT
	    "Kirk and Spock go on an undercover mission to steal the Romulan"
	    "cloaking device.  They succeed and the Romulan Commander is"
	    "inadvertantly transported to the Enterprise.") )
	 ( (TITLE "And the Children Shall Lead") (AUTHOR "Lakso" "Edward J.")
	   (SEASON 3) (POSITION-IN-SEASON 5) (ABSOLUTE-NUMBER 59)
	   (CHARACTERS
	    "Gorgon"
	    "Kirk"
	    "Spock"
	    "Mary Janowski"
	    "Tommy Starnes"
	    "Steve O'Connel") (CAST ("Gorgon" "Melvin Belli")) (LOCATIONS "Triacus")
	   (STAR-DATES 5027.3) (SHIPS)
	   (ABSTRACT
	    "On landing on Triacus, they find the Starnes expedition have"
	    "all committed suicide and all that are left are children who"
	    "are controlled by the evil Gorgan.  Kirk uses tapes of the parents"
	    "to make the children care again and destroy the Gorgan.") )
	 ( (TITLE "Spock's Brain") (AUTHOR "Coon" "Gene L.") (SEASON 3)
	   (POSITION-IN-SEASON 6) (ABSOLUTE-NUMBER 60)
	   (CHARACTERS "Kara" "Kirk" "Spock" "McCoy") (LOCATIONS "Sigma Draconis")
	   (STAR-DATES 5431.4) (SHIPS)
	   (ABSTRACT
	    "The ship is invaded and everyone made unconscious.  When they"
	    "awaken, they find Spock's brain missing.  Kirk follows the"
	    "aliens and recovers the brain.  McCoy refits it using the Teacher.") )
	 ( (TITLE "Is There In Truth No Beauty") (AUTHOR "Aroeste" "Jean Lisette")
	   (SEASON 3) (POSITION-IN-SEASON 7) (ABSOLUTE-NUMBER 61)
	   (CHARACTERS "Larry Marvich" "Miranda Jones" "Kollos" "Spock" "Mendusan")
	   (CAST ("Miranda" "Diana Muldaur")) (LOCATIONS) (STAR-DATES 5630.7) (SHIPS)
	   (ABSTRACT
	    "Miranda Jones is a blind telepath translating for the alien Kollos."
	    "Marvich's jealousy makes him look at Kollos and go insane and"
	    "injure the ship.  Spock tries to guide the ship through Kollos but"
	    "is also driven insane.  Kirk gets Miranda to save him.") )
	 ( (TITLE "Empath") (AUTHOR "Muskat" "Joyce") (SEASON 3) (POSITION-IN-SEASON 8)
	   (ABSOLUTE-NUMBER 62) (CHARACTERS "Kirk" "Spock" "McCoy" "Vian" "Gem")
	   (LOCATIONS) (STAR-DATES 5121.5) (SHIPS)
	   (ABSTRACT
	    "A race can save one of two planets and they use Kirk, Spock and"
	    "McCoy as test subjects to see if Gem, an empath, can develop"
	    "compassion.  The test is to torture them and have her take the pain.") )
	 ( (TITLE "Tholian Web") (AUTHOR "Burns" "Judy" "Richards" "Chet") (SEASON 3)
	   (POSITION-IN-SEASON 9) (ABSOLUTE-NUMBER 63)
	   (CHARACTERS
	    "Uhura"
	    "Scotty"
	    "Kirk"
	    "Spock"
	    "Tholian"
	    "Chekov"
	    "Sulu"
	    "Commodore Leskane") (LOCATIONS) (STAR-DATES 5693.2)
	   (SHIPS "Defiant" "Tholian Ships")
	   (ABSTRACT
	    "The Defiant is found in an area of space in which the warp is weak."
	    "The crew died of some madness.  Kirk is stranded and Spock must"
	    "hold the ship in place until an interphase window opens.  The"
	    "Tholians attack with a web to capture the Enterprise.") )
	 ( (TITLE "For the World is Hollow and I Have Touched the Sky")
	   (AUTHOR "Vollaerts" "Rick") (SEASON 3) (POSITION-IN-SEASON 10)
	   (ABSOLUTE-NUMBER 64) (CHARACTERS "McCoy" "Natira") (LOCATIONS "Yolanda")
	   (STAR-DATES 5476.3) (SHIPS)
	   (ABSTRACT
	    "Yolanda is a spaceship whose inhabitants think of as their world."
	    "McCoy suffers from xenopolycythemia and marries Natira,"
	    "planning to stay with her on Yolanda.  When a cure is found,"
	    "he leaves her.") )
	 ( (TITLE "Day of the Dove") (AUTHOR "Bixby" "Jerome") (SEASON 3)
	   (POSITION-IN-SEASON 11) (ABSOLUTE-NUMBER 65)
	   (CHARACTERS
	    "Klingon"
	    "Kirk"
	    "Spock"
	    "Kang"
	    "Chekov"
	    "McCoy"
	    "Mara"
	    "Sulu"
	    "Uhura") (CAST ("Kang" "Michael Ansara")) (LOCATIONS) (STAR-DATES 0)
	   (SHIPS)
	   (ABSTRACT
	    "Kirk thinks that the Klingons have destroyed a colony and the"
	    "Klingon Kang thinks that Kirk has destroyed his crew.  The root"
	    "of the problem is an alien which feeds on aggression.  Kirk"
	    "convinces the Klingons and everyone laughs at the alien.") )
	 ( (TITLE "Plato's Stepchildren") (AUTHOR "Dolinsky" "Meyer") (SEASON 3)
	   (POSITION-IN-SEASON 12) (ABSOLUTE-NUMBER 66)
	   (CHARACTERS
	    "Uhura"
	    "Kirk"
	    "Spock"
	    "Chapel"
	    "Alexander"
	    "Parmen"
	    "Philana"
	    "McCoy")
	   (CAST ("Alexander" "Michael Dunn") ("Philana" "woman who played Mea 7"))
	   (LOCATIONS) (STAR-DATES 5784.2) (SHIPS)
	   (ABSTRACT
	    "The Enterprise responds to a call from a group of Greek-like"
	    "telekinetic people who need McCoy's help.  They try to keep him"
	    "and amuse themselves by humiliating the crew.  A drug gives Kirk and"
	    "Spock the same powers and they defeat the Platonians.") )
	 ( (TITLE "Wink of an Eye") (AUTHOR "Coon" "Gene L." "Heinemann" "Arthur")
	   (SEASON 3) (POSITION-IN-SEASON 13) (ABSOLUTE-NUMBER 67)
	   (CHARACTERS
	    "Deela"
	    "Scalosian"
	    "Kirk"
	    "Spock"
	    "Uhura"
	    "McCoy"
	    "Compton"
	    "Rael"
	    "Ekor"
	    "Mira") (LOCATIONS "Scalos") (STAR-DATES 5710.5) (SHIPS)
	   (ABSTRACT
	    "Deela lures the Enterprise to Scalos to provide mates for the"
	    "sterile women.  They live in a faster time mode and speed up"
	    "some members of the crew.  Spock takes a drug to speed himself"
	    "up and save the captain.") )
	 ( (TITLE "That Which Survives") (AUTHOR "Lucas" "John Meredyth") (SEASON 3)
	   (POSITION-IN-SEASON 14) (ABSOLUTE-NUMBER 68) (CHARACTERS "Losira")
	   (CAST ("Losira" "Lee Meriwether")) (LOCATIONS) (STAR-DATES 0) (SHIPS)
	   (ABSTRACT
	    "All that is left of a world is a computer replica of Losira"
	    "which feels regret and pain at being forced to kill."
	    "This it does, never-the-less, because it was left on automatic to"
	    "protect the outpost station of some civilization, previously unknown,"
	    "of which Losira was the last survivor.") )
	 ( (TITLE "Let That Be Your Last Battlefield") (AUTHOR "Crawford" "Oliver")
	   (SEASON 3) (POSITION-IN-SEASON 15) (ABSOLUTE-NUMBER 69)
	   (CHARACTERS "Spock" "Chekov" "Sulu" "Scotty" "Uhura" "Lokai" "Bele")
	   (CAST ("Bele" "Frank Gorshin")) (LOCATIONS "Arianus" "Cheron")
	   (STAR-DATES 5730.2) (SHIPS "Starfleet Shuttlecraft" "Alien Scout Vessel")
	   (ABSTRACT
	    "The Enterprise encounters two aliens - mirror images of each other,"
	    "half black and half white.  One of them tries to take over the"
	    "ship but Kirk threatens to blow it up first.  The aliens are left"
	    "on a destroyed planet where they will spend their lives trying to"
	    "destroy one another.") )
	 ( (TITLE "Whom Gods Destroy") (AUTHOR "Erwin" "Lee") (SEASON 3)
	   (POSITION-IN-SEASON 16) (ABSOLUTE-NUMBER 70)
	   (CHARACTERS "Marta" "Lord Garth") (CAST ("prison commandant" "Keye Luke"))
	   (LOCATIONS) (STAR-DATES 5718.3) (SHIPS)
	   (ABSTRACT
	    "Kirk and Spock transport down to a prison asylum planet to deliver"
	    "medicine.  Garth, a former Starship commander, has taken over the"
	    "asylum, and, using shape-changing powers, captures Kirk and Spock."
	    "Garth attempts to get up to the Enterprise, but is"
	    "foiled by a password based on a chess move, pawn to queens level 8.") )
	 ( (TITLE "Mark of Gideon") (AUTHOR "Slavin" "George F." "Adams" "Stanley")
	   (SEASON 3) (POSITION-IN-SEASON 17) (ABSOLUTE-NUMBER 71) (CHARACTERS "Odona")
	   (LOCATIONS) (STAR-DATES 5423.4) (SHIPS)
	   (ABSTRACT
	    "Odona chooses to die of Vegan chonomeningitis as an example"
	    "to her people that voluntary death will help solve the"
	    "population problem.  Kirk, the unwitting donor of the disease, thinks"
	    "he is transporting to the council chamber of Gideon, but ends up in a"
	    "duplicate of the Enterprise, inhabited only by himself and Odona.") )
	 ( (TITLE "Lights of Zetar") (AUTHOR "Tarcher" "Jeremy" "Lewis" "Shari")
	   (SEASON 3) (POSITION-IN-SEASON 18) (ABSOLUTE-NUMBER 72)
	   (CHARACTERS "Scotty" "Mira Romaine") (LOCATIONS) (STAR-DATES 5725.3) (SHIPS)
	   (ABSTRACT
	    "Scotty falls in love with Mira Romaine, an intelligent and"
	    "technically-minded woman.  The Enterprise is attacked by a group"
	    "mentality, which inhabits Mira.  The group mind is expelled by      "
	    "placing her in a decompression chamber.") )
	 ( (TITLE "Cloud Minders") (AUTHOR "Armen" "Margaret") (SEASON 3)
	   (POSITION-IN-SEASON 19) (ABSOLUTE-NUMBER 73) (CHARACTERS)
	   (CAST ("Mayor of Stratus" "Jeff Corey")) (LOCATIONS) (STAR-DATES 5818.4)
	   (SHIPS)
	   (ABSTRACT
	    "A cultural and social oppression is encountered where the"
	    "elite live in sky cities floating above the harsh surface of"
	    "their planet, while the worker-class are forced to dig zenite"
	    "from the mines.  The gas makes them unhappy and the society"
	    "is calmed down by the addition of gas masks to protect the"
	    "workers.") )
	 ( (TITLE "Way to Eden") (AUTHOR "Richards" "Michael" "Heinemann" "Arthur")
	   (SEASON 3) (POSITION-IN-SEASON 20) (ABSOLUTE-NUMBER 74) (CHARACTERS "Spock")
	   (LOCATIONS) (STAR-DATES 5832.3) (SHIPS)
	   (ABSTRACT
	    "The son of an important person leads a group of hippies in search"
	    "of Eden.  They take over the Enterprise and go to an Eden in"
	    "which the plants are acidous.") )
	 ( (TITLE "Requiem for Methusulah") (AUTHOR "Bixby" "Jerome") (SEASON 3)
	   (POSITION-IN-SEASON 21) (ABSOLUTE-NUMBER 75)
	   (CHARACTERS "Reena Kapec" "Kirk" "Flint") (LOCATIONS) (STAR-DATES 5843.7)
	   (SHIPS)
	   (ABSTRACT
	    "The ship stops at a planet to pick up medicine to combat a plague."
	    "The owner of the planet is a man who was originally a Roman soldier"
	    "and has since been many famous, creative people.  He has fallen in"
	    "love with his android, Reena, who dies trying to choose between him"
	    "and Kirk.  Spock makes Kirk forget his love for Reena.") )
	 ( (TITLE "Savage Curtain") (AUTHOR "Roddenberry" "Gene" "Heinemann" "Arthur")
	   (SEASON 3) (POSITION-IN-SEASON 22) (ABSOLUTE-NUMBER 76)
	   (CHARACTERS "Surak" "Spock" "Kirk" "Lincoln") (LOCATIONS)
	   (STAR-DATES 5906.4) (SHIPS)
	   (ABSTRACT
	    "An apparition resembling Abraham Lincoln appears in space near the"
	    "Enterprise.  A simulacrum of Lincoln then appears onboard, and"
	    "Kirk and Spock are involved in a fight between good and evil."
	    "Spock, Lincoln, and Surak, a legendary Vulcan.  Evil is a group of"
	    "The actual planet is molten rock.") )
	 ( (TITLE "All Our Yesterdays") (AUTHOR "Aroeste" "Jean") (SEASON 3)
	   (POSITION-IN-SEASON 23) (ABSOLUTE-NUMBER 77)
	   (CHARACTERS "Spock" "Zarabeth" "McCoy" "Atoz")
	   (CAST ("Zarabeth" "Marriet Hartley") ("Atoz" "Ian Wolfe"))
	   (LOCATIONS "Sarpeidon") (STAR-DATES 5943.7) (SHIPS)
	   (ABSTRACT
	    "Spock and McCoy become caught in the past of a planet which will"
	    "become nova in their time.  Spock takes on the characteristics of a"
	    "primitive Vulcan and falls in love with Zarabeth.  He has to return"
	    "to allow McCoy to return but Zarabeth must stay behind alone.") )
	 ( (TITLE "Turnabout Intruder") (AUTHOR "Singer" "Arthur H.") (SEASON 3)
	   (POSITION-IN-SEASON 24) (ABSOLUTE-NUMBER 78)
	   (CHARACTERS "Kirk" "Dr. Janice Lester" "Spock" "McCoy" "Dr. Arthur Coleman")
	   (LOCATIONS "Camus II") (STAR-DATES 5928.5) (SHIPS)
	   (ABSTRACT
	    "An old girl friend of Kirk's who failed the Academy transfers Kirk's"
	    "mind to her body and takes over the Enterprise.  Spock uses a mind"
	    "meld to discover Kirk and helps him get back the ship.") ) 
)

       ))
