module Language where

data Program = Prog [Definition]

data Definition = FFunD [Pvar] Term
                | GFunD (PEvar, PEvar, [Pvar], Term)
                        (PAvar, [Pvar], Term)

data Pterm = FAppPT [Pexp]
           | GAppPT [Pexp]
           | IfPT Avar Avar Term TErm
           | ExpPT Pexp

data Pexp = ConsPE Pexp Pexp
          | VarPE Evar
          | PAexp' PAexp

data PAexp = AtomPA String
           | VarPA PAvar