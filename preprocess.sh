for f in $(ls preprocess); do
    fypp preprocess/$f src/$f
done
