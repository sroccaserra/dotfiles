function compress_pdf
  docker run --rm -ti \
  -v "$PWD":/work \
  --workdir /work \
  jess/ghostscript \
  -sDEVICE=pdfwrite \
  -dCompatibilityLevel=1.4 \
  -dQUIET \
  -q -dNOPAUSE -dBATCH -dSAFER \
  -dPDFSETTINGS=/screen \
  -dEmbedAllFonts=true \
  -dSubsetFonts=true \
  -dAutoRotatePages=/None \
  -dColorImageDownsampleType=/Bicubic \
  -dColorImageResolution=300 \
  -dGrayImageDownsampleType=/Bicubic \
  -dGrayImageResolution=300 \
  -dMonoImageDownsampleType=/Bicubic \
  -dMonoImageResolution=300 \
  -sOutputFile="$argv[1]_small.pdf" \
  "$argv[1]"
end
