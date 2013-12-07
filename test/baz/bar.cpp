int bar(int f) {
  if (f == 1)
    return 1;
  else if (f == 0)
    return 0;
  else
    return bar(f-2) + bar(f-1);
}
