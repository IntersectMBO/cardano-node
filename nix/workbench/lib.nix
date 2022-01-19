lib:
with lib;
{
  readJSONMay = fp:
    let fv = __tryEval (__readFile fp);
    in if fv.success
       then __fromJSON fv.value
       else {};
}
