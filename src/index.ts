/* -------------------------------------------------------------------------- */
/*                                Utility Types                               */
/* -------------------------------------------------------------------------- */

// check if a type is null or undefined
type IsNullableOrUndefineable<T> = null extends T ? true : undefined extends T ? true : false

// Removes all possible index signatures from a type
type FilterIndexSignatureType<T> = string extends T
  ? never
  : number extends T
  ? never
  : symbol extends T
  ? never
  : T

// Remove the index signature from a type
type RemoveIndexSignature<T> = {
  [K in keyof T as FilterIndexSignatureType<K>]: T[K]
}

// check if a type has an index signature
type HasIndexSignature<T> = string extends keyof T
  ? true
  : number extends keyof T
  ? true
  : symbol extends keyof T
  ? true
  : false

type GetValidIndexSignature<T> = Exclude<keyof T, symbol> extends infer K
  ? K extends string | number
    ? K
    : never
  : never

// Keys have to be strings or number and can't contain a dot since we won't be able to differ between and key with a dot and a nested key
type RemoveInvalidDotPathKeys<T> = T extends symbol
  ? never
  : T extends number
  ? T
  : T extends string
  ? // eslint-disable-next-line @typescript-eslint/no-unused-vars
    T extends `${infer _K}.${infer _R}`
    ? never
    : T
  : never

// check if a type is an array
type IsArray<T> = T extends Array<any> ? true : false

// check if a array is empty
type IsEmptyArray<T> = T extends [] ? true : false

// Get the type of an array element
type GetArrayElement<T> = T extends Array<infer U> ? U : never

/**
 * check if a type is any
 * @link https://stackoverflow.com/a/49928360/1490091
 */
type IsAny<T> = 0 extends 1 & T ? true : false

// check if a type is never
type IsNever<T> = [T] extends [never] ? true : false

/**
 * check if a type is unknown
 * @link https://github.com/sindresorhus/type-fest
 */
type IsUnknown<T> = IsNever<T> extends true
  ? false
  : IsAny<T> extends true
  ? false
  : unknown extends T
  ? true
  : false

// check if a type is a primitive
type IsPrimitive<T> = T extends string | number | boolean | bigint | symbol | undefined | null
  ? true
  : false

// remove null and undefined from a type
type ExcludeNullUndefined<T> = Exclude<T, null | undefined>

// check if a type is a tuple
type IsTuple<T> = T extends [any, ...any[]] ? true : false

// get the length of a tuple
type TupleLength<T> = T extends { length: infer L } ? (L extends number ? L : never) : never

// get the type of a tuple element
type TupleElement<T, N> = N extends keyof T ? T[N] : never

// get all numbers from 0 to L
type NumbersToZero<
  IterationCarry extends unknown[],
  DepthCarry extends unknown[]
> = TupleLength<DepthCarry> extends 0
  ? any
  : TupleLength<IterationCarry> extends 0
  ? never
  :
      | NumbersToZero<MinusOne<IterationCarry>, MinusOne<DepthCarry>>
      | TupleLength<MinusOne<IterationCarry>>

// possible record keys
type RecordKeys = string | number | symbol

// remove readonly from members of a record
type Writeable<T> = {
  -readonly [K in keyof T]: T[K]
}

/* -------------------------------------------------------------------------- */
/*                                 Math Types                                 */
/* -------------------------------------------------------------------------- */
// We use a tuple to carry our value

type MinusOne<N extends unknown[]> = N extends [...infer U, unknown] ? U : never

type BuildTuple<L extends number, T extends unknown[] = []> = T extends { length: L }
  ? T
  : BuildTuple<L, [...T, unknown]>

/* -------------------------------------------------------------------------- */
/*                                 Path Types                                 */
/* -------------------------------------------------------------------------- */

// get all possible paths of a type
type GetRecordPaths<
  T,
  DepthCarry extends unknown[],
  K extends keyof T = keyof T
> = K extends keyof T
  ? RemoveInvalidDotPathKeys<K> | `${RemoveInvalidDotPathKeys<K>}.${Path<T[K], DepthCarry>}`
  : never

// get all possible paths of an array
type GetArrayPaths<T, DepthCarry extends unknown[]> = `${number}.${Path<
  GetArrayElement<T>,
  DepthCarry
>}`

// get all possible paths of a tuple
type GetTuplePaths<T extends unknown[], DepthCarry extends unknown[]> = NumbersToZero<
  T,
  DepthCarry
> extends infer R
  ? R extends number
    ? R | `${R}.${Path<TupleElement<T, R>, DepthCarry>}`
    : never
  : never

type PathStep<T, Depth extends unknown[]> = IsAny<T> extends true
  ? string
  : IsUnknown<T> extends true
  ? never
  : IsPrimitive<T> extends true
  ? never
  : IsTuple<T> extends true
  ? T extends unknown[]
    ? GetTuplePaths<T, Depth>
    : never
  : IsArray<T> extends true
  ? (IsEmptyArray<T> extends true ? never : number) | GetArrayPaths<T, Depth>
  : HasIndexSignature<T> extends true
  ? GetValidIndexSignature<T> | GetRecordPaths<RemoveIndexSignature<T>, Depth>
  : GetRecordPaths<T, Depth>

// Final path type
type Path<T, DepthCarry extends unknown[]> = TupleLength<DepthCarry> extends 0
  ? IsPrimitive<T> extends true
    ? never
    : any
  : T extends T
  ? PathStep<Writeable<ExcludeNullUndefined<T>>, MinusOne<DepthCarry>>
  : never

type PathEntry<T, Depth extends number = 25> = Path<T, BuildTuple<Depth>>

type PathValueStep<T, P, DepthCarry extends unknown[]> = IsAny<T> extends true
  ? any
  : IsUnknown<T> extends true
  ? unknown
  : IsNullableOrUndefineable<T> extends true
  ? PathValue<ExcludeNullUndefined<T>, P, DepthCarry> | undefined
  : IsTuple<T> extends true
  ? P extends `${infer H}.${infer R}`
    ? PathValue<TupleElement<T, H>, R, DepthCarry>
    : TupleElement<T, P>
  : IsArray<T> extends true
  ? // eslint-disable-next-line @typescript-eslint/no-unused-vars
    P extends `${infer _H}.${infer R}`
    ? PathValue<GetArrayElement<T>, R, DepthCarry> | undefined
    : GetArrayElement<T> | undefined
  : P extends `${infer H}.${infer R}`
  ? H extends keyof T
    ? PathValue<T[H], R, DepthCarry> | (HasIndexSignature<T> extends true ? undefined : never)
    : never
  : P extends keyof T
  ? T[P] | (HasIndexSignature<T> extends true ? undefined : never)
  : never

// nearly same function as PathValueEntry, but without constraints for P so it is easier to use in PathValueStep
type PathValue<T, P, DepthCarry extends unknown[]> = TupleLength<DepthCarry> extends 0
  ? IsPrimitive<T> extends true
    ? never
    : unknown
  : T extends T
  ? PathValueStep<Writeable<T>, P, MinusOne<DepthCarry>>
  : never

// final path value type
type PathValueEntry<T, P extends PathEntry<T, Depth>, Depth extends number = 25> = PathValue<
  T,
  P,
  BuildTuple<Depth>
>

/**
 * Retrives a value from an object by dot notation
 *
 * @param object - object to get value from
 * @param path - path to value
 */
function getByPath<T extends Record<RecordKeys, unknown> | unknown[], P extends PathEntry<T, 25>>(
  object: T,
  path: P
): PathValueEntry<T, P, 25> {
  const pathArray = (path as string).split('.')

  return pathArray.reduce(
    (accumulator: any, current) => accumulator?.[current],
    object
  ) as PathValueEntry<T, P, 25>
}

/**
 * Sets a value in an object by dot notation
 * @param object - object to set value in
 * @param path - path to value
 * @param value - value to set
 */
function setByPath<
  T extends Record<RecordKeys, unknown> | unknown[],
  P extends PathEntry<T, 25>,
  V extends PathValueEntry<T, P, 25>
>(object: T, path: P, value: V): void {
  const pathArray = (path as string).split('.')
  const lastKey = pathArray.pop()

  if (lastKey === undefined) {
    throw new Error('Path is empty')
  }

  const objectToSet = pathArray.reduce(
    (accumulator: any, current) => accumulator?.[current],
    object
  )

  if (objectToSet === undefined) {
    throw new Error('Path is invalid')
  }

  objectToSet[lastKey] = value
}

export type { PathEntry as Path, PathValueEntry as PathValue }

export { getByPath, setByPath }
