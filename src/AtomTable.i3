INTERFACE AtomTable;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    putString(string: TEXT): CARDINAL;
    findString(string: TEXT): CARDINAL;
    findId(id: CARDINAL): TEXT;
  END;

PROCEDURE new(): T;

END AtomTable.
