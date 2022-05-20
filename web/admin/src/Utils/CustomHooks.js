// borrowed from https://dev.to/bytebodger/constructors-in-functional-components-with-hooks-280m
import { useRef } from 'react';

export const useConstructor = (callBack) => {
   const notfirst = useRef(false);
   if (notfirst.current)
      return;
   callBack();
   notfirst.current = true;
};