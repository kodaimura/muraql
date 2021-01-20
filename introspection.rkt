#lang racket

(provide introspection-resolvers
         __typename-resolver
         __schema-resolver
         __type-resolver
         __typename-fielddef
         __schema-fielddef
         __type-fielddef
         introspection-typedefs)

(define __typename-resolver
  (lambda (parent args info)
    (hash-ref info 'parenttype)))


(define __schema-resolver
  (lambda (parent args info)
    (define schema (hash-ref info 'schema))
    (hash 'types (send schema get-types)
          'queryType (send schema get-querytype)
          'mutationType (send schema get-mutationtype)
          'subscriptionType (send schema get-subscriptiontype))))


(define __type-resolver
  (lambda (parent args info)
    (define schema (hash-ref info 'schema))
    (define name (hash-ref args 'name))
    (send schema get-type (string->symbol name))))


(define introspection-resolvers
  (hash
   '__Type (hash
            'kind (lambda (parent args info)
                    (hash-ref parent 'kind 'null))

            'name (lambda (parent args info)
                    (hash-ref parent 'name 'null))

            'description (lambda (parent args info)
                           (hash-ref parent 'description 'null))

            'fields (lambda (parent args info)
                      (if (eq? (hash-ref parent 'kind) 'OBJECT)
                          (hash-values (hash-ref parent 'fields))
                          'null))

            'ofType (lambda (parent args schema)
                      (hash-ref parent 'type 'null))

            'possibleTypes (lambda (parent args info)
                             (define schema (hash-ref info 'schema))
                             (if (eq? (hash-ref parent 'kind) 'UNION)
                                 (map (lambda (name)
                                        (send schema get-type name))
                                      (hash-ref parent 'types))
                                 'null))

            'enumValues (lambda (parent args info)
                          (if (eq? (hash-ref parent 'kind) 'ENUM)
                              (map (lambda (val)
                                     (hash 'name val))
                                   (hash-keys (hash-ref parent 'values)))
                              'null))

            'inputFields (lambda (parent args info)
                           (if (eq? (hash-ref parent 'kind) 'INPUT_OBJECT)
                               (hash-values (hash-ref parent 'fields)) 
                               'null)))

   '__Field (hash
             'name (lambda (parent args info)
                    (hash-ref parent 'name 'null))
             
             'description (lambda (parent args info)
                            (hash-ref parent 'description 'null))

             'args (lambda (parent args schema)
                     (hash-ref parent 'args null))

             'type (lambda (parent args info)
                     (define schema (hash-ref info 'schema))
                     (define type (hash-ref parent 'type))
                     (if (eq? (hash-ref type 'kind) 'NAMED)
                         (send schema get-type (hash-ref type 'name))
                         type)))

   '__InputValue (hash
                  'type (lambda (parent args info)
                          (define schema (hash-ref info 'schema))
                          (define type (hash-ref parent 'type))
                          (if (eq? (hash-ref type 'kind) 'NAMED)
                              (send schema get-type (hash-ref type 'name))
                              type))
                          
                  'name (lambda (parent args info)
                          (hash-ref parent 'name 'null))

                  'description (lambda (parent args info)
                                 (hash-ref parent 'description 'null))

                  'defaultValue (lambda (parent args info)
                                  (hash-ref parent 'defaultValue 'null)))

   'EnumValue (hash
               'description 'null)))


(define __typename-fielddef
  #hash((args . ())
        (description . null)
        (name . __typename)
        (type . #hash((kind . NAMED)
                      (name . String)))))


(define __schema-fielddef
  #hash((args . ())
        (description . null)
        (name . __schema)
        (type . #hash((kind . NON_NULL)
                      (type . #hash((kind . NAMED)
                                    (name . __Schema)))))))


(define __type-fielddef
  #hash((args
         .
         (#hash((defaultValue . null)
                (name . name)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . String))))))))
        (description . null)
        (name . __type)
        (type . #hash((kind . NAMED)
                      (name . __Type)))))



(define introspection-typedefs

'(#hash((description
         .
         "The `Int` scalar type represents non-fractional
 signed whole numeric values. Int can represent values
between -(2^31) and 2^31 - 1.")
        (kind . SCALAR)
        (name . Int))

  #hash((description
        .
        "The `Float` scalar type represents signed
double-precision fractional values as specified by
[IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point).")
        (kind . SCALAR)
        (name . Float))

  #hash((description
        .
        "The `String` scalar type represents textual data,
 represented as UTF-8 character sequences.
The String type is most often used by GraphQL
 to represent free-form human-readable text.")
        (kind . SCALAR)
        (name . String))

  #hash((description
         .
         "The `Boolean` scalar type represents `true` or `false`.")
        (kind . SCALAR)
        (name . Boolean))

  #hash((description
        .
        "The `ID` scalar type represents a unique identifier,
often used to refetch an object or as key for a cache.
 The ID type appears in a JSON response as a String; however,
it is not intended to be human-readable.
When expected as an input type, any string (such as `\"4\"`) or
integer (such as `4`) input value will be accepted as an ID.")
        (kind . SCALAR)
        (name . ID))


  #hash((args . ())
        (kind . ENUM)
        (name . __TypeKind)
        (values . ("NON_NULL" "LIST" "INPUT_OBJECT" "ENUM"
                              "UNION" "INTERFACE" "OBJECT" "SCALAR")))
  
  
  #hash((description . null)
        (fields
         .
         (#hash((args . ())
                (description . null)
                (name . description)
                (type . #hash((kind . NAMED)
                              (name . String))))
          
          #hash((args . ())
                (description . null)
                (name . name)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . String))))))))
        (kind . OBJECT)
        (name . __EnumValue))

  
  #hash((description . null)
        (fields
         .
         (#hash((args . ())
                (description . null)
                (name . defaultValue)
                (type . #hash((kind . NAMED)
                              (name . String))))
          
          #hash((args . ())
                (description . null)
                (name . type)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . __Type))))))
          
          #hash((args . ())
                (description . null)
                (name . description)
                (type . #hash((kind . NAMED)
                              (name . String))))
          
          #hash((args . ())
                (description . null)
                (name . name)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . String))))))))
        (kind . OBJECT)
        (name . __InputValue))

  
  #hash((description . null)
        (fields
         .
         (#hash((args . ())
                (description . null)
                (name . type)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . __Type))))))
          #hash((args . ())
                (description . null)
                (name . args)
                (type
                 .
                 #hash((kind . NON_NULL)
                       (type
                        .
                        #hash((kind . LIST)
                              (type
                               .
                               #hash((kind . NON_NULL)
                                     (type
                                      .
                                      #hash((kind . NAMED)
                                            (name . __InputValue))))))))))
          
          #hash((args . ())
                (description . null)
                (name . description)
                (type . #hash((kind . NAMED)
                              (name . String))))
          
          #hash((args . ())
                (description . null)
                (name . name)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . String))))))))
        (kind . OBJECT)
        (name . __Field))

  
  #hash((description . null)
        (fields
         .
         (#hash((args . ())
                (description . null)
                (name . ofType)
                (type . #hash((kind . NAMED)
                              (name . __Type))))
          
          #hash((args . ())
                (description . null)
                (name . inputFields)
                (type
                 .
                 #hash((kind . LIST)
                       (type . #hash((kind . NON_NULL)
                                     (type . #hash((kind . NAMED)
                                                   (name . __InputValue))))))))
          
          #hash((args . ())
                (description . null)
                (name . enumValues)
                (type
                 .
                 #hash((kind . LIST)
                       (type . #hash((kind . NON_NULL)
                                     (type . #hash((kind . NAMED)
                                                   (name . __EnumValue))))))))
          
          #hash((args . ())
                (description . null)
                (name . possibleTypes)
                (type . #hash((kind . LIST)
                              (type . #hash((kind . NON_NULL)
                                            (type . #hash((kind . NAMED)
                                                          (name . __Type))))))))
          
          #hash((args . ())
                (description . null)
                (name . fields)
                (type . #hash((kind . LIST)
                              (type . #hash((kind . NON_NULL)
                                            (type . #hash((kind . NAMED)
                                                          (name . __Field))))))))
          
          #hash((args . ())
                (description . null)
                (name . description)
                (type . #hash((kind . NAMED)
                              (name . String))))
          
          #hash((args . ())
                (description . null)
                (name . name)
                (type . #hash((kind . NAMED)
                              (name . String))))
          
          #hash((args . ())
                (description . null)
                (name . kind)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . __TypeKind))))))))
        (kind . OBJECT)
        (name . __Type))

  
  #hash((description . null)
        (fields
         .
         (#hash((args . ())
                (description . null)
                (name . subscriptionType)
                (type . #hash((kind . NAMED)
                              (name . __Type))))
          
          #hash((args . ())
                (description . null)
                (name . mutationType)
                (type . #hash((kind . NAMED)
                              (name . __Type))))
          
          #hash((args . ())
                (description . null)
                (name . queryType)
                (type . #hash((kind . NON_NULL)
                              (type . #hash((kind . NAMED)
                                            (name . __Type))))))
          
          #hash((args . ())
                (description . null)
                (name . types)
                (type
                 .
                 #hash((kind . NON_NULL)
                       (type
                        .
                        #hash((kind . LIST)
                              (type . #hash((kind . NON_NULL)
                                            (type . #hash((kind . NAMED)
                                                          (name . __Type))))))))))))
        (kind . OBJECT)
        (name . __Schema))))

