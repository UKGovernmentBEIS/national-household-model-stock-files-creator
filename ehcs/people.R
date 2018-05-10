people.make <- function(peopleEntries){
    person.make <- function(people){
        if(!is.na(people$age) & !is.na(people$Sex)){
            data.table(
                persono = people$persno,
                age = people$age,
                sex = GET.SEX(people$Sex),
                smoker = FALSE)
        }
    }
    
    peopleEntries <- data.table(peopleEntries)
    people <- peopleEntries[, person.make(.SD), by=aacode]

    people <- ALLOCATE.SMOKERS(people)

    print(paste("people DTO complete; number of records:",nrow(people), sep=" "))
    people[, .(aacode, persono, age, sex, smoker)]
}


ALLOCATE.SMOKERS <- function(people){
    ## Make 17% of people smokers - requested by Ian Hamilton (UCL) as replacement for using now
    ## removed disables/CIGNOW variable from EHS2010
    all.people.count <- nrow(people)

    ## Give each person a unique id
    people$temp.id = as.numeric(rownames(people))

    ## Assign percentage of people as smokers
    sample.size <- round(all.people.count * 0.17,0)
    set.seed(42)
    smokers <- sample(people, sample.size)
    smokers$smoker <- TRUE

    ## Join the smoker/non-smoker subsets
    all.people <- subset(people, !temp.id %in% smokers$temp.id)
    all.people <- rbind(all.people,smokers)

    ## Check we haven't changed the total number of people
    stopifnot(all.people.count == nrow(all.people))

    all.people[order(aacode)]
}


GET.SEX <- function(Sex){
    as.factor(checked.revalue(
        Sex,c(
                "Female" = "FEMALE"
               ,"Male" = "MALE")))}
