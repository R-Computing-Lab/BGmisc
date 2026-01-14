# Create an mxModel for a pedigree

buildPedigreeModelCovariance <- function(vars = c(ad2 = 0.5,
                                           dd2 = 0.3,
                                           cn2 = 0.2, ce2 = 0.4,
                                           mt2 = 0.1,
                                           am2 = 0.25,
                                           ee2 = 0.6),
                                         Vad = TRUE,
                                         Vdd = FALSE,
                                         Vcn = TRUE,
                                         Vce = TRUE,
                                         Vmt = TRUE,
                                         Vam = FALSE,
                                         Ver = TRUE
) {
  mxModel(
    "ModelOne",
    if (Vad) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$ad2, labels = "vad", name = "Vad", lbound = 1e-10)
    },
    if (Vdd) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$dd2, labels = "vdd", name = "Vdd", lbound = 1e-10)
    },
    if (Vcn) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$cn2, labels = "vcn", name = "Vcn", lbound = 1e-10)
    },
    if (Vce) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$ce2, labels = "vce", name = "Vce", lbound = 1e-10)
    },
    if (Vmt) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$mt2, labels = "vmt", name = "Vmt", lbound = 1e-10)
    },
    if (Vam) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$am2, labels = "vam", name = "Vam", lbound = 1e-10)
    },
    if (Ver) {
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
        values = vars$ee2, labels = "ver", name = "Ver", lbound = 1e-10)
    }
  )
}


buildOneFamilyGroup <- function(
  group_name,
  Addmat,
  Nucmat,
  Extmat,
  Mtdmat,
  Amimat,
  Dmgmat,
  full_df_row,
  ytemp
) {
  fsize <- nrow(Addmat)

  mxModel(
    name = group_name,
    mxMatrix("Iden", nrow = fsize, ncol = fsize, name = "I"),
    mxMatrix("Unit", nrow = fsize, ncol = fsize, name = "U"),
    mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Addmat), name = "A"),
    # mxMatrix("Symm", nrow=fsize, ncol=fsize, values=as.matrix(Dmgmat), name="D"),
    mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Nucmat), name = "Cn"),
    # mxMatrix("Symm", nrow=fsize, ncol=fsize, values=as.matrix(Extmat), name="Ce"),
    # mxMatrix("Symm", nrow=fsize, ncol=fsize, values=as.matrix(Amimat), name="Am"),
    mxMatrix("Symm", nrow = fsize, ncol = fsize, values = as.matrix(Mtdmat), name = "Mt"),
    mxData(observed = full_df_row, type = "raw", sort = FALSE),
    mxMatrix("Full", nrow = 1, ncol = fsize, name = "M", free = TRUE, labels = "meanLI",
      dimnames = list(NULL, ytemp)),
    mxAlgebra(
      (A %x% ModelOne.Vad)
      # + (D %x% ModelOne.Vdd)
      + (Cn %x% ModelOne.Vcn)
        + (U %x% ModelOne.Vce)
        # + (Ce %x% ModelOne.Vce)
        + (Mt %x% ModelOne.Vmt)
        # + (Am %x% ModelOne.Vam)
        + (I %x% ModelOne.Ver),
      name = "V", dimnames = list(ytemp, ytemp)
    ),
    mxExpectationNormal(covariance = "V", means = "M"),
    mxFitFunctionML()
  )
}


build_family_groups <- function(
  dat, ytemp,
  Addmat, Nucmat, Extmat, Mtdmat, Amimat, Dmgmat,
  prefix = "fam"
) {
  numfam <- nrow(dat)
  groups <- vector("list", numfam)

  for (afam in seq_len(numfam)) {
    full_df_row <- matrix(dat[afam, ], nrow = 1, dimnames = list(NULL, ytemp))
    groups[[afam]] <- build_one_family_group(
      group_name  = paste0(prefix, afam),
      Addmat      = Addmat,
      Nucmat      = Nucmat,
      Extmat      = Extmat,
      Mtdmat      = Mtdmat,
      Amimat      = Amimat,
      Dmgmat      = Dmgmat,
      full_df_row = full_df_row,
      ytemp       = ytemp
    )
  }

  groups
}

buildPedigreeMx <- function(model_name, vars, group_models) {
  group_names <- vapply(group_models, function(m) m$name, character(1))
  mxModel(
    model_name,
    build_variance_components(vars),
    group_models,
    mxFitFunctionMultigroup(group_names)
  )
}
