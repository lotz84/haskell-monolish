{ mkDerivation,
  fetchFromGitHub,
  cmake,
  gcc,
  gfortran,
  openblas,
  mkl,
}:

mkDerivation {
  name = "monolish";
  src = fetchFromGitHub {
    owner = "ricosjp";
    repo = "monolish";
    rev = "0.14.2";
    sha256 = "0lb15lcsjqsj6als9zzh02lnv3i3xdzq7x4h67r8rmlizj1kiqqf";
  };
  nativeBuildInputs = [ cmake gcc gfortran ];
  # buildInputs = [ openblas ];
  buildInputs = [ mkl ];

  MKLROOT = mkl;
  patchPhase = ''
    echo 'patch phase'
    sed -i "42a\  $MKLROOT/lib" cmake/FindMKL.cmake
    sed -i "40alist(APPEND CMAKE_FIND_LIBRARY_SUFFIXES .so.1)" cmake/FindMKL.cmake
  '';

  doCheck = false;
}

