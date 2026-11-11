import random
random.seed(1337)

def generate_random_cpr():
    """Generér et CPR-nummer i formatet xxxxxx-yyyy"""
    day = f"{random.randint(1, 28):02d}"  # Dag 01-28 (for simplicity, undgår månedslængdeproblemer)
    month = f"{random.randint(1, 12):02d}"  # Måned 01-12
    year = f"{random.randint(50, 99):02d}"  # Fødselsår (1950-1999)
    control_number = f"{random.randint(1000, 9999):04d}"  # Tilfældig 4-cifret kontrolnummer
    return f"{day}{month}{year}-{control_number}"

def generate_kundeoplysninger(filename, num_records):
    navne = [
        "Michael Nielsen", "Anne Jensen", "John Doe", "Alice Smith", "Peter Petersen", "Sara Hansen",
        "Tommy Andersen", "Emma Olsen", "William Brown", "Olivia Johnson", "Lucas Miller", "Sophia Davis",
        "Alexander Wilson", "Charlotte Taylor", "Benjamin Moore", "Amelia Thomas", "James White", "Emily Harris",
        "Daniel Martin", "Isabella Clark", "Lucas Rodriguez", "Mia Lewis", "Jacob Hall", "Grace Walker",
        "Ethan Lee", "Ava Adams", "Henry King", "Lily Scott"
    ]
    adresser = [
        "Hovedgaden 12", "Strandvejen 45", "Elm Street 5", "Maple Avenue 89", "Birkevej 7", "Rosenvej 3",
        "Parkvej 23", "Skovbakken 7", "Enghave 34", "Sondergade 10", "Vestergade 15", "Norregade 2"
    ]
    lande = ["DK", "US", "GB", "DE", "FR", "ES", "SE", "NO", "IT", "CA"]

    with open(filename, "w", encoding='cp1252') as f:
        for _ in range(num_records):
            kunde_id = generate_random_cpr()
            navn = random.choice(navne)
            fødselsdato = kunde_id[:6]  # Brug de første 6 tegn fra CPR som fødselsdato
            adresse = random.choice(adresser)
            land = random.choice(lande)
            # Sørg for, at alle felter står på faste positioner
            record = (
                f"{kunde_id:<11}"  # Kunde-ID (11 tegn for CPR-format inkl. bindestreg)
                f"{navn:<20}"      # Navn (20 tegn)
                f"{fødselsdato:<10}"    # Fødselsdato (10 tegn)
                f"{adresse:<30}"        # Adresse (30 tegn)
                f"{land:<2}"            # Land (2 tegn)
                "\n"
            )
            f.write(record)

def generate_sanction_list(filename, num_records):
    sanction_ids = [f"S{i:03d}" for i in range(1, num_records + 1)]
    navne = [
        "Michael Nilsen", "Anne J.", "Jonathan Doe", "Alicia Smith", "John Smith", "Sara H.",
        "Tom Anderson", "Emma Olson", "William Brown", "Olivia Johnson", "Lucas Miller", "Sophia Davis",
        "Alexander Wilson", "Charlotte Taylor", "Benjamin Moore", "Amelia Thomas", "James White", "Emily Harris",
        "Daniel Martin", "Isabella Clark", "Lucas Rodriguez", "Mia Lewis", "Jacob Hall", "Grace Walker"
    ]
    aliaser_pool = [
        "Michael N.", "M.N.", "Mikael", "Mickey", "Mike",
        "Anne J.", "Annie J.", "A. Jensen", "Anna", "Anita",
        "Jonathan Doe", "J. Doe", "Johnny", "Jon D.", "John",
        "Alicia Smith", "Ali Smith", "Alice S.", "A.S.", "Lisa Smith",
        "John Smith", "J.S.", "Johnny Smith", "John S.", "Smith J."
    ]
    fødselsdatoer = ["1980-05-15", "1992-03-22", "1975-11-02", "1988-01-10", "1990-07-15", "1995-07-14",
                     "1982-09-29", "1978-11-11", "1985-06-18", "1993-02-25", "1977-10-03", "1965-04-12"]
    lande = ["DK", "US", "GB", "DE", "FR", "ES", "NO", "SE", "IT", "CA"]

    with open(filename, "w") as f:
        for _ in range(num_records):
            sanction_id = random.choice(sanction_ids)
            navn = random.choice(navne)
            alias_count = random.randint(1, 5)  # Vælg et tilfældigt antal aliaser (1-5)
            alias_list = random.sample(aliaser_pool, alias_count) + ["-"] * (5 - alias_count)  # Fyld op med tomme felter
            fødselsdato = random.choice(fødselsdatoer)
            land = random.choice(lande)
            # Sørg for, at alle felter står på faste positioner
            record = (
                f"{sanction_id:<5}"     # Sanction-ID (5 tegn)
                f"{navn:<20}"          # Navn (20 tegn)
                f"{alias_list[0]:<20}" # Alias1 (20 tegn)
                f"{alias_list[1]:<20}" # Alias2 (20 tegn)
                f"{alias_list[2]:<20}" # Alias3 (20 tegn)
                f"{alias_list[3]:<20}" # Alias4 (20 tegn)
                f"{alias_list[4]:<20}" # Alias5 (20 tegn)
                f"{fødselsdato:<10}"   # Fødselsdato (10 tegn)
                f"{land:<2}"           # Land (2 tegn)
                "\n"
            )
            f.write(record)

# Generér inputfiler
generate_kundeoplysninger("KundeOplysninger.txt", 500)
generate_sanction_list("SanctionList.txt", 200)